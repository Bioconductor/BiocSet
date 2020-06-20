.OBOSet_check_ancestry <- function(object) {
    errors <- character()

    if (!("children" %in% names(.set(object)))) {
        msg <- "'children' needs to be included in the obo file."
        errors <- c(errors, msg)
    }

    if (!("parents" %in% names(.set(object)))) {
        msg <- "'parents' needs to be included in the obo file."
        errors <- c(errors, msg)
    }

    if (!("ancestors" %in% names(.set(object)))) {
        msg <- "'ancestors' needs to be included in the obo file."
        errors <- c(errors, msg)
    }

    errors
}

#' @importFrom tibble is_tibble
.OBOSet_check_metadata <- function(object) {
    errors <- character()

    obo_header <- metadata(object)$obo_header
    if (!is_tibble(obo_header))
        errors <- c(errors, "'metadata$obo_header' must be a tibble")

    if (!all(c("key", "value") %in% names(obo_header))) {
        msg <- "'key', 'value' must be columns of 'metadata$obo_header"
        errors <- c(errors, msg)
    }

    errors
}

.OBOSet_validity <- function(object) {
    errors <- .OBOSet_check_ancestry(object)
    errors <- .OBOSet_check_metadata(object)

    if (length(errors)) errors else TRUE
}

#' @rdname OBOSet-class
#' @name OBOSet
#' @aliases OBOSet-class
#' @title OBOSet class
#' @description A class representing the 'OBO' file format as a BiocSet.
#' @exportClass OBOSet
NULL

.OBOSet <- setClass("OBOSet", contains = "BiocSet", validity = .OBOSet_validity)

#' @rdname OBOSet-class
#' @param elementset A tibble with element set information.
#' @param element A tibble with element information.
#' @param set A tibble with set information.
#' @param metadata A tibble with key-value pairs describing OBO file
#'     format header data
#' @return An S4 \code{OBOOSet} object. OBO sets conform to the 'obo'
#'     file format, with OBO 'Term' entries corresponding to
#'     elements. Parent / child relationships (e.g., 'is_a') are
#'     summarized as 'parents', 'ancestors', and 'children' character
#'     list columns of 'set'.
#' @examples
#' OBOSet()
#' oboFile <- system.file(package = "BiocSet", "extdata", "sample_go.obo")
#' import(oboFile)
#' @export
OBOSet <- function(elementset, element, set, metadata)
{
    if (missing(metadata)) {
        obo_header <- tibble(key = "format-version", value = "1.2")
        metadata <- list(obo_header = obo_header)
    }

    es <- BiocSet_from_elementset(elementset, element, set, metadata)
    .OBOSet(es)
}
