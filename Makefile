doc:
	R -e "library(devtools); document('.')"

test:
	mkdir -p cran/betalink
	cp -r * cran/betalink 2>/dev/null; true
	rm -r cran/betalink/{cran,inst,tests}
	rm cran/betalink/Makefile
	cd cran; R CMD check --as-cran betalink
