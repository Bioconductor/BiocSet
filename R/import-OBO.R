check_ancestry <- function(object) {
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

    if (length(errors) == 0) TRUE else errors
}

.OBOSet <- setClass("OBOSet", contains = "BiocSet", validity = check_ancestry)

.OBOFile = setClass("OBOFile", contains = "RTLFile")

OBOFile = function(resource, ...)
    .OBOFile(resource = resource)

#' @importFrom ontologyIndex get_ontology
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr rename
import.obo <- function(path, extract_tags = "minimal") {
    namespace <- id <- element <- NULL
    stopifnot(extract_tags %in% c("minimal", "everything"))
    if (extract_tags == "everything") {
        obo <- get_ontology(path, extract_tags = "everything")
        elements <- as_tibble(lapply(obo, as.vector)) %>%
            filter(!namespace %in% "external") %>%
            rename(element = id)
    } else {
        obo <- get_ontology(path)
        elements <- as_tibble(lapply(obo, as.vector)) %>%
            rename(element = id)
    }

    children <- name <- parents <- ancestors <- set <- NULL
    sets <- 
        elements %>%
        filter(lengths(children) > 0) %>%
        select(element, name, parents, children, ancestors) %>%
        rename(set = element)

    myunnest <- function(tbl) {
        tibble(
            set = rep(tbl$set, lengths(tbl$element)),
            element = unlist(tbl$element)
        )
    }

    elementsets <- 
        elements %>%
        filter(lengths(children) > 0) %>%
        rename(set = element) %>%
        select(set, element = children) %>%
        myunnest

    oboset <- BiocSet_from_elementset(elementsets, elements, sets)
    .OBOSet(oboset)
}

#' @rdname import
#' @export
#' @examples
#'
#' oboFile <- system.file(package = "BiocSet", "extdata", "sample_go.obo")
#' tst_obo <- import(oboFile)
setMethod("import", c("OBOFile", "ANY", "ANY"),
    function(con, format, text, ...)
{
    import.obo(resource(con), ...)
})

#' @importFrom tidyr unnest
oboset_element_children <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    oboset %>%
        es_element() %>%
        select(c("element", "children")) %>%
        unnest("children")
}

oboset_element_parents <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    oboset %>%
        es_element() %>%
        select(c("element", "parents")) %>%
        unnest("parents")
}

oboset_element_ancestors <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    oboset %>%
        es_element() %>%
        select(c("element", "ancestors")) %>%
        unnest("ancestors")
}

oboset_set_children <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    oboset %>%
        es_set() %>%
        select(c("set", "children")) %>%
        unnest("children")
}

oboset_set_parents <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    oboset %>%
        es_set() %>%
        select(c("set", "parents")) %>%
        unnest("parents")
}

oboset_set_ancestors <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    oboset %>%
        es_set() %>%
        select(c("set", "ancestors")) %>%
        unnest("ancestors")
}
