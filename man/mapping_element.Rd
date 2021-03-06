% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping_element.R
\name{mapping_element}
\alias{mapping_element}
\alias{map_unique}
\alias{map_multiple}
\alias{map_add_element}
\title{Functions for mapping elements in the element tibble to different id types}
\usage{
map_unique(es, org, from, to)

map_multiple(
  es,
  org,
  from,
  to,
  multi = c("list", "filter", "asNA", "CharacterList")
)

map_add_element(es, org, from, add)
}
\arguments{
\item{es}{The BiocSet objec to map the elements on.}

\item{org}{The AnnotationDbi object to identify keys/mappings from.}

\item{from}{A character to indicate which identifier to map from.}

\item{to}{A character to indicate which identifier to map to.}

\item{multi}{How should multiple values be returned? 
Options include:     
\itemize{
\item{list: This will just return a list object to the end user.}
\item{filter: This will remove all elements that contain multiple 
matches and will therefore return a shorter vector than what came in 
whenever some of the keys match more than one value.}
\item{asNA: This will return an NA value whenever there are multiple 
matches.}
\item{CharacterList: This just returns a SimpleCharacterList object.}
\item{FUN: A function can be supplied to the 'multiVals' argument 
for custom behaviors.}
}}

\item{add}{The id to add to the \code{BiocSet} object.}
}
\value{
For \code{map_unique}, a \code{BiocSet} object with unique 
   elements.

For \code{map_multiple}, a \code{BiocSet} object with multiple 
    mappings for certain elements.

For \code{map_add_element}, a \code{BiocSet} object with a new column
    in the element tibble with the mapping of the new id type.
}
\description{
Functions for dealing with unique mapping and multiple mapping.
    \code{map_add_element} will add the mapping as a new column instead of 
    overwriting the current one used for the mapping.
}
\examples{
library(org.Hs.eg.db)
es <- BiocSet(set1 = c("C5", "GANC"), set2 = c("AFM", "CGB1", "ADAM32"))
map_unique(es, org.Hs.eg.db, "SYMBOL", "ENTREZID")

map_multiple(es, org.Hs.eg.db, "SYMBOL", "ENSEMBLTRANS", "asNA")

map <- map_add_element(es, org.Hs.eg.db, "SYMBOL", "ENTREZID")
es \%>\% mutate_element(entrez = map)
}
