#
#
#Thank to: http://nicercode.github.io/blog/2013-07-09-figure-functions/


to.dev <- function(expr, dev, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  dev(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}


# = for create the pdf file
to.pdf <- function(expr, filename, ...)
  to.dev(expr, pdf, filename, ...)

# = for create the png file
to.png <- function(expr, filename, ...)
to.dev(expr, png, filename)
