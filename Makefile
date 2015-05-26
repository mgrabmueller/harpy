.PHONY: all clean install reinstall uninstall doc

all:
	runhaskell Setup.hs configure --prefix=$(HOME)/
	runhaskell Setup.hs build

clean:
	runhaskell Setup.hs clean


install:
	runhaskell Setup.hs install --user

reinstall:
	runhaskell Setup.hs clean
	runhaskell Setup.hs configure --prefix=$(HOME)/
	runhaskell Setup.hs build
	runhaskell Setup.hs install --user

uninstall:
	runhaskell Setup.hs unregister --user

doc:
	runhaskell Setup.hs haddock
