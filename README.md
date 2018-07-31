
buckydown
=========

Custom pdf bookdown format

Installation
------------

You can install buckydown from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/buckydown")
```

Sources
-------

This package borrows the package infrastructure of huskydown for making a dissertation pdf in with bookdown but uses a LaTeX template I found online. All the demo text and documentation wrongly talks about the University of Washington and huskydown, so bear with me.

Testing from within this package
--------------------------------

To test the PDF template stored in `inst/` assuming we are at top level. Rebuild the package. Then:

``` r
buckydown::test_thesis_pdf()
```
