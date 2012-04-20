
FNAME=lisp-codec

.PHONY: all
all: pdf

.PHONY: pdf html ps dvi
pdf: $(FNAME).pdf
html: html/$(FNAME).html
ps: $(FNAME).ps
dvi: $(FNAME).dvi

$(FNAME).dvi: $(FNAME).tex
	latex $<
	latex $<
	latex $<

html/$(FNAME).html: $(FNAME).tex
	latex2html $< -split 0 -dir html -mkdir -local_icons -noinfo -strict \
	              -t 'RL-Glue Lisp Codec Manual' -html_version 4.0,math

$(FNAME).pdf: $(FNAME).dvi
	dvipdfm -o $@ $<

$(FNAME).ps: $(FNAME).dvi
	dvips -o $@ $<

.PHONY: clean
clean:
	-rm -rf html/*
	-rm -f *.dvi *.ps *.eps *.pdf *.aux *.log *.out *.toc *.lof

