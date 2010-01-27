DOCPATH =/usr/local/share/doc/libadacrypt-dev


.PHONY : clean
.SILENT : clean

all:
	latex acldoku
	bibtex acldoku
	latex acldoku
	bibtex acldoku
	dvips -o  acldoc-de.ps acldoku.dvi


clean:
	rm -f *.ps *.pdf *.dvi *.log *~ *.toc *.aux *.bbl *.blg

install:
	mkdir -p $(DOCPATH)
	cp  acldoku-de.ps $(DOCPATH)/acldoc-de.ps

uninstall:	
	rm -f $(DOCPATH)/acldoc-de.ps
