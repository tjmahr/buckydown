#' Create an R Markdown PDF Thesis document
#'
#' Creates a pdf dissertation.
#'
#' @export
#' @param template Path of the Pandoc LaTeX template file. Defaults to
#'   \code{"template.tex"}.
#' @param toc A Boolean specifying whether table of contents should be created
#' @param toc_depth A positive integer
#' @param toc_bib Whether to add bibliography to table of contents
#' @param toc_appendix Whether to add appendices.
#' @param toc_depth A positive integer
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "tango", "pygments", "kate", "monochrome", "espresso",
#'   "zenburn", and "haddock". Pass NULL to prevent syntax highlighting.
#' @param pandoc_args optional values for the pandoc_args argument of
#'   bookdown::pdf_book. The argument \code{"--top-level-division=chapter"} is
#'   always included in the pandoc_args.
#' @param ... other arguments to `bookdown::pdf_book()`
#' @return A pdf document
#' @import bookdown
thesis_pdf <- function(
  template = "template.tex",
  toc = TRUE, toc_depth = 3, toc_bib = TRUE, toc_appendix = TRUE,
  dev = "cairo_pdf",
  highlight = "default",
  pandoc_args = NULL, ...){
  # output:
  #   bookdown::pdf_book:
  #     # latex_engine: xelatex
  #     pandoc_args: [
  #       "--latex-engine", "xelatex"
  #     ]
  #     dev: cairo_pdf
  #     toc_appendix: true
  #     toc_bib: true
  #     template: test-template2.tex
  base <- bookdown::pdf_book(
    template = template,
    toc = toc,
    toc_depth = toc_depth,
    toc_bib = toc_bib,
    toc_appendix = toc_appendix,
    dev = dev,
    highlight = highlight,
    keep_tex = TRUE,
    pandoc_args = c(pandoc_args, "--top-level-division=chapter"),
    ...)

  # Mostly copied from knitr::render_sweave
  # base$knitr$opts_chunk$comment <- NA

  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = fix_envs)
  on.exit(options(bookdown.post.late = old_opt))

  base
}

fix_envs = function(x){
  beg_reg <- '^\\s*\\\\begin\\{.*\\}'
  end_reg <- '^\\s*\\\\end\\{.*\\}'
  i3 = if (length(i1 <- grep(beg_reg, x))) (i1 - 1)[grepl("^\\s*$", x[i1 - 1])]

  i3 = c(i3,
         if (length(i2 <- grep(end_reg, x))) (i2 + 1)[grepl("^\\s*$", x[i2 + 1])]
  )
  if (length(i3)) x = x[-i3]
  x
}

#' @export
test_thesis_pdf <- function(..., preview = FALSE) {
  dir <- tempdir()
  setwd(dir)
  rmarkdown::draft(
    'index.Rmd', template = 'thesis', package = 'buckydown',
    create_dir = TRUE, edit = FALSE
  )

  setwd('index')

  bookdown::render_book(
    'index.Rmd',
    buckydown::thesis_pdf(...),
    preview = preview)

  file.path(dir, "index")
}
