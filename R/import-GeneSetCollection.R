setClass("GeneSetCollection")

#' as("BiocSet", "GeneSetCollection")
#'
#' @name coerce
#' @aliases coerce,BiocSet,GeneSetCollection-method
#' @importFrom methods setAs coerce
#' @exportMethod coerce
setAs("BiocSet", "GeneSetCollection", function(from) {
    if(!requireNamespace("GSEABase")) {
        stop("Please install GSEABase")
    }
    lst <- as.list(from)
    gsl <- Map(function(ids, name) {
        GSEABase::GeneSet(ids, setName = name)
        }, lst, names(lst))
    GSEABase::GeneSetCollection(gsl)
})
