

.PHONY: document
document:
	Rscript -e "devtools::document()"

.PHONY: install
install:
	make document
	Rscript -e "devtools::install()"

.PHONY: docs
docs:
	-rm -rf docs
	Rscript -e "pkgdown::build_site()"

# Reto; for testing only
upload:
	rsync -va docs/* retostauffer:~/html/trash/annex/

test:
	Rscript -e "devtools::load_all(); tinytest::test_all()"

.PHONY: all
all:
	make document
	make install
	make docs
	make upload
