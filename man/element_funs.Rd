% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/element_funs.R
\name{element_funs}
\alias{element_funs}
\alias{filter_element}
\alias{select_element}
\alias{mutate_element}
\alias{summarise_element}
\alias{arrange_element}
\alias{left_join_element}
\alias{tibble_from_element}
\alias{data.frame_from_element}
\title{Functions applied to elements in a \code{BiocSet} object}
\usage{
filter_element(.data, ...)

select_element(.data, ...)

mutate_element(.data, ...)

summarise_element(.data, ...)

arrange_element(.data, ...)

left_join_element(.data, ...)

tibble_from_element(.data, how = unlist)

data.frame_from_element(.data, how = unlist)
}
\arguments{
\item{.data}{A \code{BiocSet} object.}

\item{...}{Additional arguments passed to the function.}

\item{how}{Multiple entries will become a list.}
}
\value{
A \code{BiocSet} object.

For \code{tibble_from_element}, a tibble.

For \code{data.frame_from_element}, a data.frame.
}
\description{
All of the major methods applied to a \code{BiocSet} object can
    be explicitly applied to the element tibble. These functions bypass the
    need to use the \code{es_activate} function by indicating what function
    should be used on the element tibble.
}
\examples{
es <- BiocSet(set1 = letters, set2 = LETTERS)
filter_element(es, element == "a")

es \%>\% select_element(element)

es \%>\% mutate_element(pval = rnorm(1:52))

es \%>\% summarise_element(n = n())

es \%>\% arrange_element(desc(element))

tbl <- tibble(x = 1:5, y = letters[1:5])
es <- BiocSet(set1 = letters[c(1,3,5)], set2 = letters[c(2,4)])
left_join_element(es, tbl, by = c(element = "y"))

tibble_from_element(es)

data.frame_from_element(es)
}
