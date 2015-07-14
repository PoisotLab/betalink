doc:
	R -e "library(devtools); document('.')"

cran/betalink:
	mkdir -p cran/betalink

test: cran/betalink doc
	mkdir -p cran/betalink
	cp -r * cran/betalink 2>/dev/null; true
	rm -r cran/betalink/{cran,inst,tests}
	rm cran/betalink/Makefile
	cd cran; R CMD check betalink

betalink.tar.gz:
	cd cran; tar -zcvf $@ betalink
	mv cran/betalink.tar.gz $@
