#' Extract or Replace Matched Substrings
#' 
#' Extract or replace matched substrings from match data 
#' obtained by \code{\link[base]{regexpr}}, \code{\link[base]{gregexpr}}
#' or \code{\link[base]{regexec}}.
#' 
#' @param x a character vector
#' @param m an object with match data
#' @param invert a logical: if TRUE, extract the non-matched substrings.
#' @param value an object with suitable replacment values for the matched
#' or non-matched substrings (see Details)
#' 
#' @section Details:
#' If invert is TRUE (default), regmatches extracts the matched 
#' substrings as specified by the match data. For vector match 
#' data (as obtained from regexpr), empty matches are dropped;
#'  for list match data, empty matches give empty components 
#'  (zero-length character vectors).
#'
#' If invert is FALSE, regmatches extracts the non-matched 
#' substrings, i.e., the strings are split according to the
#'  matches similar to strsplit (for vector match data, at 
#'  most a single split is performed).
#'
#' Note that the match data can be obtained from regular 
#' expression matching on a modified version of x with the 
#' same numbers of characters.
#'
#' The replacement function can be used for replacing the
#' matched or non-matched substrings. For vector match data, 
#' if invert is TRUE, value should be a character vector with
#' length the number of matched elements in m. Otherwise, it 
#' should be a list of character vectors with the same length 
#' as m, each as long as the number of replacements needed. 
#' Replacement coerces values to character or list and 
#' generously recycles values as needed. Missing replacement
#' values are not allowed.
#'    
#' @section Value:
#' For regmatches, a character vector with the matched 
#' substrings if m is a vector and invert is FALSE. Otherwise,
#' a list with the matched or non-matched substrings.
#' 
#' For regmatches<-; the updated character vector.
#' 
#' @rdname regmatches
#' @export
#' @examples
#' x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
#' pattern <- "[[:space:]]*(,|and)[[:space:]]"
#' ## Match data from regexpr()
#' m <- regexpr(pattern, x)
#' regmatches(x, m)
#' regmatches(x, m, invert = TRUE)
#' ## Match data from gregexpr()
#' m <- gregexpr(pattern, x)
#' regmatches(x, m)
#' regmatches(x, m, invert = TRUE)
#' 
#' ## Consider
#' x <- "John (fishing, hunting), Paul (hiking, biking)"
#' ## Suppose we want to split at the comma (plus spaces) between the
#' ## persons, but not at the commas in the parenthesized hobby lists.
#' ## One idea is to "blank out" the parenthesized parts to match the
#' ## parts to be used for splitting, and extract the persons as the
#' ## non-matched parts.
#' ## First, match the parenthesized hobby lists.
#' m <- gregexpr("\\([^)]*\\)", x)
#' ## Write a little utility for creating blank strings with given numbers
#' ## of characters.
#' blanks <- function(n) {
#'     vapply(Map(rep.int, rep.int(" ", length(n)), n, USE.NAMES = FALSE),
#'            paste, "", collapse = "")
#' }
#' ## Create a copy of x with the parenthesized parts blanked out.
#' s <- x
#' regmatches(s, m) <- Map(blanks, lapply(regmatches(s, m), nchar))
#' s
#' ## Compute the positions of the split matches (note that we cannot call
#' ## strsplit() on x with match data from s).
#' m <- gregexpr(", *", s)
#' ## And finally extract the non-matched parts.
#' regmatches(x, m, invert = TRUE)
regmatches <- function (x, m, invert = FALSE){
    if (length(x) != length(m)) 
        stop(gettextf("%s and %s must have the same length", 
                      sQuote("x"), sQuote("m")), domain = NA)
    ili <- is.list(m)
    useBytes <- if (ili) 
        any(unlist(lapply(m, attr, "useBytes")))
    else any(attr(m, "useBytes"))
    if (useBytes) {
        asc <- iconv(x, "latin1", "ASCII")
        ind <- is.na(asc) | (asc != x)
        if (any(ind)) 
            Encoding(x[ind]) <- "bytes"
    }
    if (!ili && !invert) {
        so <- m[ind <- (!is.na(m) & (m > -1L))]
        eo <- so + attr(m, "match.length")[ind] - 1L
        return(substring(x[ind], so, eo))
    }
    y <- if (invert) {
        Map(function(u, so, ml) {
            if ((n <- length(so)) == 1L) {
                if (is.na(so)) 
                    return(character())
                else if (so == -1L) 
                    return(u)
            }
            beg <- if (n > 1L) {
                eo <- so + ml - 1L
                if (any(eo[-n] >= so[-1L])) 
                    stop(gettextf("need non-overlapping matches for %s", 
                                  sQuote("invert = TRUE")), domain = NA)
                c(1L, eo + 1L)
            }
            else {
                c(1L, so + ml)
            }
            end <- c(so - 1L, nchar(u))
            substring(u, beg, end)
        }, x, m, if (ili) 
            lapply(m, attr, "match.length")
            else attr(m, "match.length"), USE.NAMES = FALSE)
    }
    else {
        Map(function(u, so, ml) {
            if (length(so) == 1L) {
                if (is.na(so) || (so == -1L)) 
                    return(character())
            }
            substring(u, so, so + ml - 1L)
        }, x, m, lapply(m, attr, "match.length"), USE.NAMES = FALSE)
    }
    names(y) <- names(x)
    y
}


#' @rdname regmatches
#' @usage regmatches(x, m, invert = FALSE) <- value
#' @export
`regmatches<-` <- function (x, m, invert = FALSE, value) 
{
    if (!length(x)) 
        return(x)
    y <- regmatches(x, m, !invert)
    ili <- is.list(m)
    if (!ili && !invert) {
        value <- as.character(value)
        if (any(is.na(value))) 
            stop("missing replacement values are not allowed")
        pos <- which(sapply(y, length) == 2L)
        np <- length(pos)
        nv <- length(value)
        if (np != nv) {
            if (!nv) 
                stop("must have replacement values for matches")
            value <- rep(value, length.out = np)
        }
        x[pos] <- paste(sapply(y, `[`, 1L), value, sapply(y, 
                                                          `[`, 2L), sep = "")
        return(x)
    }
    value <- lapply(value, as.character)
    if (any(is.na(unlist(value)))) 
        stop("missing replacement values are not allowed")
    if (!length(value)) 
        stop("value does not provide any replacement values")
    value <- rep(value, length.out = length(x))
    y <- if (invert) {
        Map(function(u, v) {
            nu <- length(u)
            nv <- length(v)
            if (nv != (nu + 1L)) {
                if (!nv) 
                    stop("must have replacements for non-matches")
                v <- rep(v, length.out = nu + 1L)
            }
            paste0(v, c(u, ""), collapse = "")
        }, y, value, USE.NAMES = FALSE)
    }
    else {
        Map(function(u, v) {
            nu <- length(u)
            nv <- length(v)
            if (nv != (nu - 1L)) {
                if (!nv) 
                    stop("must have replacements for matches")
                v <- rep(v, length.out = nu - 1L)
            }
            paste0(u, c(v, ""), collapse = "")
        }, y, value, USE.NAMES = FALSE)
    }
    y <- unlist(y)
    names(y) <- names(x)
    y
}