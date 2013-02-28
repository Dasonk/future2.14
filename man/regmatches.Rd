\name{regmatches}
\alias{regmatches}
\alias{regmatches<-}
\title{Extract or Replace Matched Substrings}
\usage{
  regmatches(x, m, invert = FALSE)

  regmatches(x, m, invert = FALSE) <- value
}
\arguments{
  \item{x}{a character vector}

  \item{m}{an object with match data}

  \item{invert}{a logical: if TRUE, extract the non-matched
  substrings.}

  \item{value}{an object with suitable replacment values
  for the matched or non-matched substrings (see Details)}
}
\description{
  Extract or replace matched substrings from match data
  obtained by \code{\link[base]{regexpr}},
  \code{\link[base]{gregexpr}} or
  \code{\link[base]{regexec}}.
}
\section{Details}{
  If invert is TRUE (default), regmatches extracts the
  matched substrings as specified by the match data. For
  vector match data (as obtained from regexpr), empty
  matches are dropped; for list match data, empty matches
  give empty components (zero-length character vectors).

  If invert is FALSE, regmatches extracts the non-matched
  substrings, i.e., the strings are split according to the
  matches similar to strsplit (for vector match data, at
  most a single split is performed).

  Note that the match data can be obtained from regular
  expression matching on a modified version of x with the
  same numbers of characters.

  The replacement function can be used for replacing the
  matched or non-matched substrings. For vector match data,
  if invert is TRUE, value should be a character vector
  with length the number of matched elements in m.
  Otherwise, it should be a list of character vectors with
  the same length as m, each as long as the number of
  replacements needed. Replacement coerces values to
  character or list and generously recycles values as
  needed. Missing replacement values are not allowed.
}

\section{Value}{
  For regmatches, a character vector with the matched
  substrings if m is a vector and invert is FALSE.
  Otherwise, a list with the matched or non-matched
  substrings.

  For regmatches<-; the updated character vector.
}
\examples{
x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
pattern <- "[[:space:]]*(,|and)[[:space:]]"
## Match data from regexpr()
m <- regexpr(pattern, x)
regmatches(x, m)
regmatches(x, m, invert = TRUE)
## Match data from gregexpr()
m <- gregexpr(pattern, x)
regmatches(x, m)
regmatches(x, m, invert = TRUE)

## Consider
x <- "John (fishing, hunting), Paul (hiking, biking)"
## Suppose we want to split at the comma (plus spaces) between the
## persons, but not at the commas in the parenthesized hobby lists.
## One idea is to "blank out" the parenthesized parts to match the
## parts to be used for splitting, and extract the persons as the
## non-matched parts.
## First, match the parenthesized hobby lists.
m <- gregexpr("\\\\([^)]*\\\\)", x)
## Write a little utility for creating blank strings with given numbers
## of characters.
blanks <- function(n) {
    vapply(Map(rep.int, rep.int(" ", length(n)), n, USE.NAMES = FALSE),
           paste, "", collapse = "")
}
## Create a copy of x with the parenthesized parts blanked out.
s <- x
regmatches(s, m) <- Map(blanks, lapply(regmatches(s, m), nchar))
s
## Compute the positions of the split matches (note that we cannot call
## strsplit() on x with match data from s).
m <- gregexpr(", *", s)
## And finally extract the non-matched parts.
regmatches(x, m, invert = TRUE)
}

