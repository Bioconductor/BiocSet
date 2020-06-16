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

export.BiocSet_to_obo <- function(tbl, path = tempfile(fileext = ".obo")) {
    stopifnot(.is_tbl_elementset(es_elementset(tbl)))
    
    elements <- es_element(tbl)
    ## better to use colnames() or names()?
    if (!"id" %in% names(elements))
        names(elements) <- sub("element", "id", names(elements))

    if (!"is_a" %in% names(elements))
        names(elements) <- sub("parents", "is_a", names(elements))

    if ("obsolete" %in% names(elements) & is.logical(elements$obsolete)) {
        names(elements) <- sub("obsolete", "is_obsolete", names(elements))
        elements$is_obsolete <- as.numeric(elements$is_obsolete)
    }

    term_tags <- c("id", "is_anonymous", "name", "namespace", "alt_id", "def", 
        "comment", "subset", "synonym", "xref", "builtin", "property_value", 
        "is_a", "intersection_of", "union_of", "equivalent_to", "disjoint_from", 
        "relationship", "created_by", "creation_date", "is_obsolete", 
        "replaced_by", "consider")

    sub_elements <- elements[,names(elements) %in% term_tags]
     
    #titles <- names(sub_elements)
    subjects <- sub_elements$id

    terms <- lapply(seq_along(subjects), function(x) {
        less_elements <- sub_elements[x, which(!is.na(sub_elements[x,]))]
        titles <- names(less_elements)
        lens <- sapply(seq_along(titles), function(y) {
            lengths(less_elements[, y][[1]])
        })
        expand_titles <- rep(titles, lens)
        paste0("\n[Terms]\n", expand_titles, ": ", unlist(less_elements))
    })

}

#' @rdname import
#' @export
#' 
#' fl <- system.file("extdata", "sample_go.obo", package = "BiocSet")
#' tbl <- import(fl)
#' new_fl <- tempfile(fileext = ".obo")
#' obo <- export(tbl, new_fl)
setMethod(
    "export", c("BiocSet", "OBOFile", "ANY"),
    function(object, con, format, ...)
{
    export.BiocSet_to_obo(object, resource(con))
})
