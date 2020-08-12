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

BiocSet_from_GeneSetCollection <- function(gsc) {
    if(!requireNamespace("GSEABase", quietly = TRUE)) {
        stop("Please install GSEABase")
    }
    BiocSet(GSEABase::geneIds(gsc))
}
