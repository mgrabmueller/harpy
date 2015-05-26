all: larger-tutorial.pdf

larger-tutorial.pdf: larger-tutorial.tex 

larger-tutorial.tex: larger-tutorial.lhs
	lhs2TeX $< > $@

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

clean:
	rm -f larger-tutorial.aux larger-tutorial.toc\
 larger-tutorial.tex larger-tutorial.log larger-tutorial.out\
 larger-tutorial.ptb

really-clean: clean
	rm -f larger-tutorial.pdf
