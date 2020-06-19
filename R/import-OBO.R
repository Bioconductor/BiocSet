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
.import_obo <- function(path, extract_tags = "minimal") {
    stopifnot(extract_tags %in% c("minimal", "everything"))
    if (extract_tags == "everything") {
        obo <- get_ontology(path, extract_tags = "everything")
        elements <- as_tibble(lapply(obo, as.vector)) %>%
            filter(!.data$namespace %in% "external") %>%
            rename(element = "id")
    } else {
        obo <- get_ontology(path)
        elements <- as_tibble(lapply(obo, as.vector)) %>%
            rename(element = "id")
    }

    sets <- 
        elements %>%
        filter(lengths(.data$children) > 0) %>%
        select("element", "name", "parents", "children", "ancestors") %>%
        rename(set = "element")

    myunnest <- function(tbl) {
        tibble(
            set = rep(tbl$set, lengths(tbl$element)),
            element = unlist(tbl$element)
        )
    }

    elementsets <- 
        elements %>%
        filter(lengths(.data$children) > 0L) %>%
        rename(set = "element") %>%
        select("set", element = "children") %>%
        myunnest()

    ## elements not in any set are in their own set
    identity <-
        elements %>%
        select("element") %>%
        anti_join(elementsets, by = "element") %>%
        mutate(set = .data$element)
    elementsets <-
        elementsets %>%
        bind_rows(identity)

    oboset <- BiocSet_from_elementset(elementsets, elements, sets)
    .OBOSet(oboset, metadata = list(obo_header = attr(obo, "version")))
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
    .import_obo(resource(con), ...)
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

.export_obo <- function(tbl, path) {
    stopifnot(
        .is_tbl_elementset(es_elementset(tbl)),
        `'path' exists` = !file.exists(path)
    )
    
    elements <- es_element(tbl)
    ## better to use colnames() or names()?
    if (!"id" %in% names(elements))
        elements <- elements %>% rename(id = "element")

    if (!"is_a" %in% names(elements))
        elements <- elements %>% rename(is_a = "parents")

    if ("obsolete" %in% names(elements) & is.logical(elements$obsolete)) {
        elements <-
            elements %>%
            rename(is_obsolete = "obsolete") %>%
            mutate(is_obsolete = as.integer(.data$is_obsolete))
    }

    term_tags <- c(
        "id", "is_anonymous", "name", "namespace", "alt_id", "def",
        "comment", "subset", "synonym", "xref", "builtin", "property_value", 
        "is_a", "intersection_of", "union_of", "equivalent_to", "disjoint_from", 
        "relationship", "created_by", "creation_date", "is_obsolete", 
        "replaced_by", "consider"
    )
    ## keep only known columns
    elements <- elements[names(elements) %in% term_tags]

    ## term-key-value tibble
    kv <- tibble(
        term = rep(seq_len(nrow(elements)), ncol(elements)),
        key = rep(names(elements), each = nrow(elements)),
        value = unlist(elements, recursive = FALSE, use.names = FALSE)
    )
    ## 'unlist' value
    long <- tibble(
        term = rep(kv$term, lengths(kv$value)),
        key = rep(kv$key, lengths(kv$value)),
        value = unlist(kv$value)
    )
    
    tkv <-
        long %>%
        ## drop NAs
        filter(!is.na(.data$value)) %>%
        ## arrange by term then order in term_tags
        arrange(.data$term, factor(.data$key, levels = term_tags))

    records <- split(paste(tkv$key, tkv$value, sep=": "), tkv$term)
    con <- file(path, "at")
    ## write header lines
    writeLines(metadata(tbl)$obo_header, con)
    ## write each record
    for (record in records)
        writeLines(c("", "[Term]", record), con)
    close(con)

    invisible(path)
}

#' @rdname import
#' @examples
#' fl <- system.file("extdata", "sample_go.obo", package = "BiocSet")
#' tbl <- import(fl)
#' new_fl <- tempfile(fileext = ".obo")
#' obo <- export(tbl, new_fl)
#' @export
setMethod(
    "export", c("BiocSet", "OBOFile", "ANY"),
    function(object, con, format, ...)
{
    .export_obo(object, resource(con))
})
