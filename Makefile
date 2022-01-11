.PHONY: doc

doc:
	Rscript -e "roxygen2::roxygenize()"
