doc:
	Rscript -e "library(devtools); document('.')"

cran/betalink:
	mkdir -p cran/betalink

check: cran/betalink doc
	mkdir -p cran/betalink
	cp -r * cran/betalink 2>/dev/null; true
	rm -r cran/betalink/{cran,inst,tests}
	rm cran/betalink/Makefile
	rm cran/betalink/*.gz
	cd cran; R CMD check --as-cran betalink

test: doc
	Rscript -e "library(devtools); test('.')"

betalink.tar.gz:
	cd cran; tar -zcvf $@ betalink
	mv cran/betalink.tar.gz $@
