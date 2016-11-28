{-# OPTIONS -cpp #-}

--------------------------------------------------------------------------
-- |
-- Module:      Harpy.CodeGenMonad
-- Copyright:   (c) 2006-20015 Martin Grabmueller and Dirk Kleeblatt
-- License:     BSD3
-- 
-- Maintainer:  martin@grabmueller.de
-- Stability:   provisional
-- Portability: portable (but generated code non-portable)
--
-- Monad for generating x86 machine code at runtime.
--
-- This is a combined reader-state-exception monad which handles all
-- the details of handling code buffers, emitting binary data,
-- relocation etc.
--
-- All the code generation functions in module "Harpy.X86CodeGen" live
-- in this monad and use its error reporting facilities as well as the
-- internal state maintained by the monad.  
--
-- The library user can pass a user environment and user state through
-- the monad.  This state is independent from the internal state and
-- may be used by higher-level code generation libraries to maintain
-- their own state across code generation operations.
-- --------------------------------------------------------------------------

module Harpy.CodeGenMonad(
    -- * Types
          CodeGen,
          ErrMsg,
          RelocKind(..),
          Reloc,
          Label,
          FixupKind(..),
          CodeGenConfig(..),
          firstBuffer,
          defaultCodeGenConfig,
    -- * Functions
    -- ** General code generator monad operations
          failCodeGen,
    -- ** Accessing code generation internals
          getEntryPoint,
          getCodeOffset,
          getBasePtr,
          getCodeBufferList,
    -- ** Access to user state and environment
          setState,
          getState,
          getEnv,
          withEnv,
    -- ** Label management
          newLabel,
          newNamedLabel,
          setLabel,
          defineLabel,
          (@@),
          emitFixup,
          labelAddress,
          emitRelocInfo,
    -- ** Code emission
          emit8,
          emit8At,
          peek8At,
          emit32,
          emit32At,
          checkBufferSize,
          ensureBufferSize,
    -- ** Executing code generation
          runCodeGen,
          runCodeGenWithConfig,
    -- ** Calling generated functions
          callDecl,
    -- ** Interface to disassembler
          disassemble
    ) where

import qualified Harpy.X86Disassembler as Dis

import Control.Applicative
import Control.Monad

import Text.PrettyPrint.HughesPJ

import Numeric

import Data.List
import qualified Data.Map as Map
import Foreign
import Foreign.C.Types
import System.IO

import Control.Monad.Trans

import Language.Haskell.TH.Syntax


-- | An error message produced by a code generation operation.
type ErrMsg = Doc

-- | The code generation monad, a combined reader-state-exception
-- monad.
newtype CodeGen e s a = CodeGen ((e, CodeGenEnv) -> (s, CodeGenState) -> IO ((s, CodeGenState), Either ErrMsg a))

-- | Configuration of the code generator.  There are currently two
-- configuration options.  The first is the number fo bytes to use for
-- allocating code buffers (the first as well as additional buffers
-- created in calls to 'ensureBufferSize'.  The second allows to pass
-- in a pre-allocated code buffer and its size.  When this option is
-- used, Harpy does not perform any code buffer resizing (calls to
-- 'ensureBufferSize' will be equivalent to calls to
-- 'checkBufferSize').
data CodeGenConfig = CodeGenConfig { 
      codeBufferSize   :: Int,                   -- ^ Size of individual code buffer blocks. 
      customCodeBuffer :: Maybe (Ptr Word8, Int) -- ^ Code buffer passed in.
    }

-- | Internal state of the code generator
data CodeGenState = CodeGenState { 
      buffer        :: Ptr Word8,                    -- ^ Pointer to current code buffer.
      bufferList    :: [(Ptr Word8, Int)],           -- ^ List of all other code buffers.
      firstBuffer   :: Ptr Word8,                    -- ^ Pointer to first buffer.
      bufferOfs     :: Int,                          -- ^ Current offset into buffer where next instruction will be stored.
      bufferSize    :: Int,                          -- ^ Size of current buffer.
      relocEntries  :: [Reloc],                      -- ^ List of all emitted relocation entries.
      nextLabel     :: Int,                          -- ^ Counter for generating labels.
      definedLabels :: Map.Map Int (Ptr Word8, Int, String), -- ^ Map of already defined labels.
      pendingFixups :: Map.Map Int [FixupEntry],     -- ^ Map of labels which have been referenced, but not defined.
      config        :: CodeGenConfig                 -- ^ Configuration record.
    }

data FixupEntry = FixupEntry { 
      fueBuffer :: Ptr Word8,
      fueOfs    :: Int,
      fueKind   :: FixupKind 
    }

-- | Kind of a fixup entry.  When a label is emitted with
-- 'defineLabel', all prior references to this label must be fixed
-- up.  This data type tells how to perform the fixup operation.
data FixupKind = Fixup8          -- ^ 8-bit relative reference
               | Fixup16         -- ^ 16-bit relative reference
               | Fixup32         -- ^ 32-bit relative reference
               | Fixup32Absolute -- ^ 32-bit absolute reference
               deriving (Show)

data CodeGenEnv = CodeGenEnv { tailContext :: Bool }
   deriving (Show)

-- | Kind of relocation, for example PC-relative
data RelocKind = RelocPCRel    -- ^ PC-relative relocation
               | RelocAbsolute -- ^ Absolute address
   deriving (Show)

-- | Relocation entry
data Reloc = Reloc { offset :: Int, 
             -- ^ offset in code block which needs relocation
                     kind :: RelocKind,
             -- ^ kind of relocation
                     address :: FunPtr () 
             -- ^ target address
           }
   deriving (Show)

-- | Label
data Label = Label Int String
           deriving (Eq, Ord)

unCg :: CodeGen e s a -> ((e, CodeGenEnv) -> (s, CodeGenState) -> IO ((s, CodeGenState), Either ErrMsg a))
unCg (CodeGen a) = a

instance Functor (CodeGen e s) where
  fmap f m = CodeGen (\ env state -> do
                         r <- unCg m env state
                         case r of
                           (state', Left err) -> return (state', Left err)
                           (state', Right v) -> return (state', Right $ f v))
  
instance Applicative (CodeGen e s) where
  pure x = cgReturn x
  f <*> x = do
    f' <- f
    x' <- x
    return $ f' x'
  
instance Monad (CodeGen e s) where
    return x = cgReturn x
    fail err = cgFail err
    m >>= k = cgBind m k

cgReturn :: a -> CodeGen e s a
cgReturn x = CodeGen (\_env state -> return (state, Right x))

cgFail :: String -> CodeGen e s a
cgFail err = CodeGen (\_env state -> return (state, Left (text err)))

cgBind :: CodeGen e s a -> (a -> CodeGen e s a1) -> CodeGen e s a1
cgBind m k = CodeGen (\env state -> 
               do r1 <- unCg m env state
                  case r1 of
                    (state', Left err) -> return (state', Left err)
                    (state', Right v) -> unCg (k v) env state')

-- | Abort code generation with the given error message.
failCodeGen :: Doc -> CodeGen e s a
failCodeGen d = CodeGen (\_env state -> return (state, Left d))

instance MonadIO (CodeGen e s) where
  liftIO st = CodeGen (\_env state -> do { r <- st; return (state, Right r) })

emptyCodeGenState :: CodeGenState
emptyCodeGenState = CodeGenState { buffer = undefined,
                                   bufferList = [],
                                   firstBuffer = undefined,
                                   bufferOfs = 0,
                                   bufferSize = 0,
                                   relocEntries = [], 
                                   nextLabel = 0,
                                   definedLabels = Map.empty,
                                   pendingFixups = Map.empty,
                                   config = defaultCodeGenConfig}

-- | Default code generation configuration.  The code buffer size is
-- set to 4KB, and code buffer management is automatic.  This value is
-- intended to be used with record update syntax, for example:
--
-- >  runCodeGenWithConfig ... defaultCodeGenConfig{codeBufferSize = 128} ...
defaultCodeGenConfig :: CodeGenConfig
defaultCodeGenConfig = CodeGenConfig { codeBufferSize = defaultCodeBufferSize,
                                       customCodeBuffer = Nothing }

defaultCodeBufferSize :: Int
defaultCodeBufferSize = 4096

-- | Execute code generation, given a user environment and state.  The
-- result is a tuple of the resulting user state and either an error
-- message (when code generation failed) or the result of the code
-- generation.  This function runs 'runCodeGenWithConfig' with a
-- sensible default configuration.
runCodeGen :: CodeGen e s a -> e -> s -> IO (s, Either ErrMsg a)
runCodeGen cg uenv ustate =
    runCodeGenWithConfig cg uenv ustate defaultCodeGenConfig

foreign import ccall "static stdlib.h"
  memalign :: CUInt -> CUInt -> IO (Ptr a)

foreign import ccall "static sys/mman.h"
  mprotect :: CUInt -> CUInt -> Int -> IO Int

-- | Like 'runCodeGen', but allows more control over the code
-- generation process.  In addition to a code generator and a user
-- environment and state, a code generation configuration must be
-- provided.  A code generation configuration allows control over the
-- allocation of code buffers, for example.
runCodeGenWithConfig :: CodeGen e s a -> e -> s -> CodeGenConfig -> IO (s, Either ErrMsg a)
runCodeGenWithConfig (CodeGen cg) uenv ustate conf =
    do (buf, sze) <- case customCodeBuffer conf of
                       Nothing -> do let initSize = codeBufferSize conf
                                     let size = fromIntegral initSize
                                     arr <- memalign 0x1000 size
                                     -- 0x7 = PROT_{READ,WRITE,EXEC}
                                     _ <- mprotect (fromIntegral $ ptrToIntPtr arr) size 0x7
                                     return (arr, initSize)
                       Just (buf, sze) -> return (buf, sze)
       let env = CodeGenEnv {tailContext = True}
       let state = emptyCodeGenState{buffer = buf,
                                     bufferList = [],
                                     firstBuffer = buf,
                                     bufferSize = sze,
                                     config = conf}
       ((ustate', _), res) <- cg (uenv, env) (ustate, state)
       return (ustate', res)

-- | Check whether the code buffer has room for at least the given
-- number of bytes.  This should be called by code generators
-- whenever it cannot be guaranteed that the code buffer is large
-- enough to hold all the generated code.  Lets the code generation
-- monad fail when the buffer overflows.
--
-- /Note:/ Starting with version 0.4, Harpy automatically checks for
-- buffer overflow, so you do not need to call this function anymore.
checkBufferSize :: Int -> CodeGen e s ()
checkBufferSize needed =
    do state <- getInternalState
       unless (bufferOfs state + needed <= bufferSize state)
              (failCodeGen (text "code generation buffer overflow: needed additional" <+> 
                            int needed <+> text "bytes (offset =" <+> 
                            int (bufferOfs state) <> 
                            text ", buffer size =" <+> 
                            int (bufferSize state) <> text ")"))

-- | Make sure that the code buffer has room for at least the given
-- number of bytes.  This should be called by code generators whenever
-- it cannot be guaranteed that the code buffer is large enough to
-- hold all the generated code.  Creates a new buffer and places a
-- jump to the new buffer when there is not sufficient space
-- available.  When code generation was invoked with a pre-defined
-- code buffer, code generation is aborted on overflow.
--
-- /Note:/ Starting with version 0.4, Harpy automatically checks for
-- buffer overflow, so you do not need to call this function anymore.
ensureBufferSize :: Int -> CodeGen e s ()
ensureBufferSize needed =
    do state <- getInternalState
       case (customCodeBuffer (config state)) of
         Nothing ->
             unless (bufferOfs state + needed + 5 <= bufferSize state)
                        (do let incrSize = max (needed + 16) (codeBufferSize (config state))
                            arr <- liftIO $ mallocBytes incrSize
                            ofs <- getCodeOffset
                            let buf = buffer state
                                disp :: Int
                                disp = arr `minusPtr` (buf `plusPtr` ofs) - 5
                            emit8 0xe9    -- FIXME: Machine dependent!
                            emit32 (fromIntegral disp)
                            st <- getInternalState
                            setInternalState st{buffer = arr, bufferList = bufferList st ++ [(buffer st, bufferOfs st)], bufferOfs = 0})
         Just (_, _) -> checkBufferSize needed

-- | Return a pointer to the beginning of the first code buffer, which
-- is normally the entry point to the generated code.
getEntryPoint :: CodeGen e s (Ptr Word8)
getEntryPoint =
    CodeGen (\ _ (ustate, state) -> 
      return $ ((ustate, state), Right (firstBuffer state)))

-- | Return the current offset in the code buffer, e.g. the offset
-- at which the next instruction will be emitted.
getCodeOffset :: CodeGen e s Int
getCodeOffset =
    CodeGen (\ _ (ustate, state) -> 
      return $ ((ustate, state), Right (bufferOfs state)))

-- | Set the user state to the given value. 
setState :: s -> CodeGen e s ()
setState st =
    CodeGen (\ _ (_, state) -> 
      return $ ((st, state), Right ()))

-- | Return the current user state.
getState :: CodeGen e s s
getState =
    CodeGen (\ _ (ustate, state) -> 
      return $ ((ustate, state), Right (ustate)))

-- | Return the current user environment.
getEnv :: CodeGen e s e
getEnv =
    CodeGen (\ (uenv, _) state -> 
      return $ (state, Right uenv))

-- | Set the environment to the given value and execute the given
-- code generation in this environment.
withEnv :: e -> CodeGen e s r -> CodeGen e s r
withEnv e (CodeGen cg) =
    CodeGen (\ (_, env) state ->
      cg (e, env) state)

-- | Set the user state to the given value. 
setInternalState :: CodeGenState -> CodeGen e s ()
setInternalState st =
    CodeGen (\ _ (ustate, _) -> 
      return $ ((ustate, st), Right ()))

-- | Return the current user state.
getInternalState :: CodeGen e s CodeGenState
getInternalState =
    CodeGen (\ _ (ustate, state) -> 
      return $ ((ustate, state), Right (state)))

-- | Return the pointer to the start of the code buffer.
getBasePtr :: CodeGen e s (Ptr Word8)
getBasePtr =
    CodeGen (\ _ (ustate, state) -> 
      return $ ((ustate, state), Right (buffer state)))

-- | Return a list of all code buffers and their respective size 
-- (i.e., actually used space for code, not allocated size).
getCodeBufferList :: CodeGen e s [(Ptr Word8, Int)]
getCodeBufferList = do st <- getInternalState
                       return $ bufferList st ++ [(buffer st, bufferOfs st)]

-- | Generate a new label to be used with the label operations
-- 'emitFixup' and 'defineLabel'.
newLabel :: CodeGen e s Label
newLabel =
    do state <- getInternalState
       let lab = nextLabel state
       setInternalState state{nextLabel = lab + 1}
       return (Label lab "")

-- | Generate a new label to be used with the label operations
-- 'emitFixup' and 'defineLabel'.  The given name is used for
-- diagnostic purposes, and will appear in the disassembly.
newNamedLabel :: String -> CodeGen e s Label
newNamedLabel name =
    do state <- getInternalState
       let lab = nextLabel state
       setInternalState state{nextLabel = lab + 1}
       return (Label lab name)

-- | Generate a new label and define it at once
setLabel :: CodeGen e s Label
setLabel =
    do l <- newLabel
       defineLabel l
       return l

-- | Emit a relocation entry for the given offset, relocation kind 
-- and target address.
emitRelocInfo :: Int -> RelocKind -> FunPtr a -> CodeGen e s ()
emitRelocInfo ofs knd addr = 
    do state <- getInternalState
       setInternalState state{relocEntries =
                              Reloc{offset = ofs, 
                                    kind = knd,
                                    address = castFunPtr addr} : 
                              (relocEntries state)}

-- | Emit a byte value to the code buffer. 
emit8 :: Word8 -> CodeGen e s ()
emit8 op = 
    CodeGen (\ _ (ustate, state) -> 
      do let buf = buffer state
             ptr = bufferOfs state
         pokeByteOff buf ptr op
         return $ ((ustate, state{bufferOfs = ptr + 1}), Right ()))

-- | Store a byte value at the given offset into the code buffer.
emit8At :: Int -> Word8 -> CodeGen e s ()
emit8At pos op = 
    CodeGen (\ _ (ustate, state) -> 
      do let buf = buffer state
         pokeByteOff buf pos op
         return $ ((ustate, state), Right ()))

-- | Return the byte value at the given offset in the code buffer.
peek8At :: Int -> CodeGen e s Word8
peek8At pos =
    CodeGen (\ _ (ustate, state) -> 
      do let buf = buffer state
         b <- peekByteOff buf pos
         return $ ((ustate, state), Right b))

-- | Like 'emit8', but for a 32-bit value.
emit32 :: Word32 -> CodeGen e s ()
emit32 op = 
    CodeGen (\ _ (ustate, state) -> 
      do let buf = buffer state
             ptr = bufferOfs state
         pokeByteOff buf ptr op
         return $ ((ustate, state{bufferOfs = ptr + 4}), Right ()))

-- | Like 'emit8At', but for a 32-bit value.
emit32At :: Int -> Word32 -> CodeGen e s ()
emit32At pos op = 
    CodeGen (\ _ (ustate, state) -> 
      do let buf = buffer state
         pokeByteOff buf pos op
         return $ ((ustate, state), Right ()))

-- | Emit a label at the current offset in the code buffer.  All
-- references to the label will be relocated to this offset.
defineLabel :: Label -> CodeGen e s ()
defineLabel (Label lab name) = 
    do state <- getInternalState
       case Map.lookup lab (definedLabels state) of
         Just _ -> failCodeGen $ text "duplicate definition of label" <+> 
                     int lab
         _ -> return ()
       case Map.lookup lab (pendingFixups state) of
         Just fixups -> do mapM_ (performFixup (buffer state) (bufferOfs state)) fixups
                           setInternalState state{pendingFixups = Map.delete lab (pendingFixups state)}
         Nothing -> return ()
       state1 <- getInternalState
       setInternalState state1{definedLabels = Map.insert lab (buffer state1, bufferOfs state1, name) (definedLabels state1)}

performFixup :: Ptr Word8 -> Int -> FixupEntry -> CodeGen e s ()
performFixup labBuf labOfs (FixupEntry{fueBuffer = buf, fueOfs = ofs, fueKind = knd}) =
    do let diff = (labBuf `plusPtr` labOfs) `minusPtr` (buf `plusPtr` ofs)
       liftIO $ case knd of
                  Fixup8  -> pokeByteOff buf ofs (fromIntegral diff - 1 :: Word8)
                  Fixup16 -> pokeByteOff buf ofs (fromIntegral diff - 2 :: Word16)
                  Fixup32 -> pokeByteOff buf ofs (fromIntegral diff - 4 :: Word32)
                  Fixup32Absolute -> pokeByteOff buf ofs (fromIntegral (ptrToWordPtr (labBuf `plusPtr` labOfs)) :: Word32)
       return ()


-- | This operator gives neat syntax for defining labels.  When @l@ is a label, the code
--
-- > l @@ mov eax ebx
--
-- associates the label l with the following @mov@ instruction.
(@@) :: Label -> CodeGen e s a -> CodeGen e s a
(@@) lab gen = do defineLabel lab
                  gen

-- | Emit a fixup entry for the given label at the current offset in
-- the code buffer (unless the label is already defined).
-- The instruction at this offset will
-- be patched to target the address associated with this label when
-- it is defined later.
emitFixup :: Label -> Int -> FixupKind -> CodeGen e s ()
emitFixup (Label lab _) ofs knd = 
    do state <- getInternalState 
       let base = buffer state
           ptr = bufferOfs state
           fue = FixupEntry{fueBuffer = base,
                            fueOfs = ptr + ofs,
                            fueKind = knd}
       case Map.lookup lab (definedLabels state) of
         Just (labBuf, labOfs, _) -> performFixup labBuf labOfs fue
         Nothing -> setInternalState state{pendingFixups = Map.insertWith (++) lab [fue] (pendingFixups state)}

-- | Return the address of a label, fail if the label is not yet defined.
labelAddress :: Label -> CodeGen e s (Ptr a)
labelAddress (Label lab name) = do
  state <- getInternalState
  case Map.lookup lab (definedLabels state) of
    Just (labBuf, labOfs, _) -> return $ plusPtr labBuf labOfs
    Nothing -> fail $ "Label " ++ show lab ++ "(" ++ name ++ ") not yet defined"


-- | Disassemble all code buffers.  The result is a list of
-- disassembled instructions which can be converted to strings using
-- the 'Dis.showIntel' or 'Dis.showAtt' functions from module
-- "Harpy.X86Disassembler".
disassemble :: CodeGen e s [Dis.Instruction]
disassemble = do
  s <- getInternalState
  let buffers = bufferList s
  r <- mapM (\ (buff, len) -> do
             r <- liftIO $ Dis.disassembleBlock buff len
             case r of
                    Left err -> cgFail $ show err
                    Right instr -> return instr
            ) $ buffers ++ [(buffer s, bufferOfs s)]
  r' <- insertLabels (concat r)
  return r'
 where insertLabels :: [Dis.Instruction] -> CodeGen e s [Dis.Instruction]
       insertLabels = liftM concat . mapM ins
       ins :: Dis.Instruction -> CodeGen e s [Dis.Instruction]
       ins i@(Dis.BadInstruction{}) = return [i]
       ins i@(Dis.PseudoInstruction{}) = return [i]
       ins i@(Dis.Instruction{Dis.address = addr}) =
           do state <- getInternalState
              let allLabs = Map.toList (definedLabels state)
                  labs = filter (\ (_, (buf, ofs, _)) -> fromIntegral (ptrToWordPtr (buf `plusPtr` ofs)) == addr) allLabs
                  createLabel (l, (buf, ofs, name)) = Dis.PseudoInstruction addr
                                                        (case name of
                                                           "" ->
                                                               "label " ++ show l ++ 
                                                                " [" ++ 
                                                                hex32 (fromIntegral (ptrToWordPtr (buf `plusPtr` ofs))) ++ 
                                                                "]"
                                                           _ -> name ++ ": [" ++ 
                                                                  hex32 (fromIntegral (ptrToWordPtr (buf `plusPtr` ofs))) ++ 
                                                                  "]")
              return $ fmap createLabel labs ++ [i]
       hex32 :: Int -> String
       hex32 i =
              let w :: Word32
                  w = fromIntegral i
                  s = showHex w ""
              in take (8 - length s) (repeat '0') ++ s

#ifndef __HADDOCK__

callDecl :: String -> Q Type -> Q [Dec]
callDecl ns qt =  do
    t0 <- qt
    let (tvars, cxt, t) = case t0 of
                         ForallT vs c t' -> (vs, c, t')
                         _ -> ([], [], t0)
    let name = mkName ns
    let funptr = AppT (ConT $ mkName "FunPtr") t
    let ioresult = t -- addIO t
    let ty = AppT (AppT ArrowT funptr) ioresult
    dynName <- newName "conv"
    let dyn = ForeignD $ ImportF CCall Safe "dynamic" dynName $ ForallT tvars cxt ty
    vs <- mkArgs t
    cbody <- [| CodeGen (\env (ustate, state) ->
                        do let code = firstBuffer state
                           res <- liftIO $ $(do
                                             c <- newName "c"
                                             cast <- [|castPtrToFunPtr|]
                                             let f = AppE (VarE dynName)
                                                          (AppE cast
                                                                (VarE c))
                                             return $ LamE [VarP c] $ foldl AppE f $ map VarE vs
                                            ) code
                           return $ ((ustate, state), Right res))|]
    let call = ValD (VarP name) (NormalB $ LamE (map VarP vs) cbody) []
    return [ dyn, call ]

mkArgs (AppT (AppT ArrowT _from) to) = do
  v  <- newName "v"
  vs <- mkArgs to
  return $ v : vs
mkArgs _ = return []

addIO (AppT t@(AppT ArrowT _from) to) = AppT t $ addIO to
addIO t = AppT (ConT $ mkName "IO") t

#else

-- | Declare a stub function to call the code buffer. Arguments are the name
-- of the generated function, and the type the code buffer is supposed to have.
-- The type argument can be given using the [t| ... |] notation of Template Haskell.
-- Allowed types are the legal types for FFI functions.
callDecl :: String -> Q Type -> Q [Dec]

#endif
