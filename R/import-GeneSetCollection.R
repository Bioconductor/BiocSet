#' GeneSetCollection
#' @rdname genesetcollection
#' @name genesetcollection
#' @description The following functions deal with converting a \code{BiocSet}
#'     object into a \code{GeneSetCollection} object, or vice versa.
#' @param biocset The \code{BiocSet} object that will become a
#'     \code{GeneSetCollection} object.
#' @return For `GeneSetCollection_from_BiocSet()`, a GeneSetCollection.
#' @export
#' @examples
#' biocset <- BiocSet(set1 = letters, set2 = LETTERS)
#' gsc <- GeneSetCollection_from_BiocSet(biocset)
#' gsc
GeneSetCollection_from_BiocSet <- function(biocset) {
    if(!requireNamespace("GSEABase", quietly = TRUE)) {
        stop("Please install GSEABase")
    }

    lst <- as.list(biocset)
    gsl <- Map(function(ids, name) {
        GSEABase::GeneSet(ids, setName = name)
    }, lst, names(lst))
    GSEABase::GeneSetCollection(gsl)
}

#' @rdname genesetcollection
#' @param gsc The \code{GeneSetCollection} that will become a \code{BiocSet}
#'     object.
#' @return For `BiocSet_from_GeneSetCollection()`, a BiocSet object.
#' @export
#' @examples
#'
#' BiocSet_from_GeneSetCollection(gsc)
BiocSet_from_GeneSetCollection <- function(gsc) {
    if(!requireNamespace("GSEABase", quietly = TRUE)) {
        stop("Please install GSEABase")
    }
    BiocSet(GSEABase::geneIds(gsc))
}
