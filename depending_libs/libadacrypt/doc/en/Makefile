DOCPATH =/usr/local/share/doc/libadacrypt-dev


.PHONY : clean
.SILENT : clean

all:
	latex acldoku_en
	bibtex acldoku_en
	latex acldoku_en
	bibtex acldoku_en
	dvips -o acldoc-en.ps acldoku_en.dvi  


clean:
	rm -f *.ps *.ps *.dvi *.log *~ *.toc *.aux *.bbl

install:
	mkdir -p $(DOCPATH)
	cp  acldoku-de.ps $(DOCPATH)/acldoc-en.ps

uninstall:	
	rm -f $(DOCPATH)/acldoc-en.ps
