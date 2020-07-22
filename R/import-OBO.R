.OBOFile = setClass("OBOFile", contains = "RTLFile")

OBOFile = function(resource, ...)
    .OBOFile(resource = resource)

## Need to account for diff format versions, order and tags can change
## These tags are for format version 1.2
.OBO_TAGS <- list(
    "1.2" = list(
        TERMS = c(
            "id", "is_anonymous", "name", "namespace", "alt_id", "def",
            "comment", "subset", "synonym", "xref", "is_a", "intersection_of",
            "union_of", "disjoint_from", "relationship", "is_obsolete",
            "replaced_by", "consider", "created_by", "creation_date"
        )
    )
)

.obo_term_tags <- function(version) {
    if (!version %in% names(.OBO_TAGS)) {
        warning("unknown OBO version '", version, "'; using version 1.2")
        version <- "1.2"
    }
    .OBO_TAGS[[version]]$TERMS
}

#' @importFrom ontologyIndex get_ontology
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr rename anti_join
.import_obo <- function(path, extract_tags = "minimal") {
    stopifnot(
        extract_tags %in% c("minimal", "everything")
    )
    
    obo <- get_ontology(path, extract_tags = "everything")

    ## header
    version0 <- attr(obo, "version")
    pattern <- "^([^:]+):(.*)"
    obo_header <- tibble(
        key = sub(pattern, "\\1", version0),
        value = trimws(sub(pattern, "\\2", version0))
    )
    version <-
        obo_header %>%
        filter(key == "format-version") %>%
        pull("value")
    metadata <- list(obo_header = obo_header)

    ## parent / child ancestor information
    structure <- c("id", "parents", "children", "ancestors", "namespace")
    isa <-
        obo[intersect(names(obo), structure)] %>%
        lapply(function(x) unname(as.vector(x))) %>%
        as_tibble() %>%
        rename(element = "id")

    if ("namespace" %in% names(isa))
        isa <- isa %>% filter(.data$namespace != "external")

    ## elements
    minimal <- c("element", "name", "obsolete")
    tags <- c(minimal, .obo_term_tags(version))
    elements <-
        obo[intersect(names(obo), tags)] %>%
        lapply(function(x) unname(as.vector(x))) %>%
        as_tibble() %>%
        rename(element = "id")

    if ("namespace" %in% names(elements))
        elements <- elements %>% filter(.data$namespace != "external")

    if (extract_tags == "minimal")
        elements <- elements %>% select(minimal)

    ## sets
    sets <-
        elements %>%
        select(set = "element", "name")

    ## elementsets
    child <-
        isa %>%
        select(element = "element", set = "parents") %>%
        unnest("set") %>%
        mutate(is_a = TRUE)
    offspring <-
        isa %>%
        select(set = "ancestors", element = "element") %>%
        unnest("set")
    elementsets <-
        right_join(child, offspring, by = c("element", "set")) %>%
        mutate(is_a = !is.na(.data$is_a)) %>%
        select("element", "set", "is_a")

    OBOSet(elementsets, elements, sets, metadata)
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

.element_elementset <- function(oboset) {
    inner_join(es_element(oboset), es_elementset(oboset), by = "element")
}

.set_elementset <- function(oboset) {
    inner_join(es_set(oboset), es_elementset(oboset), by = "set")
}

#' @importFrom tidyr unnest
oboset_element_children <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    .element_elementset(oboset) %>%
        filter(is_a) %>%
        select("set", "element") %>%
        rename(element = "set", children = "element")
}

oboset_element_parents <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    .element_elementset(oboset) %>%
        filter(is_a) %>%
        select("element", "set") %>%
        rename(parents = "set")
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
    .set_elementset(oboset) %>%
        filter(is_a) %>%
        select("set", "element") %>%
        rename(children = "element")
}

oboset_set_parents <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    .set_elementset(oboset) %>%
        filter(is_a) %>%
        select("element", "set") %>%
        rename(set = "element", parents = "set")
}

oboset_set_ancestors <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    oboset %>%
        es_set() %>%
        select(c("set", "ancestors")) %>%
        unnest("ancestors")
}

.oboset_is_a <- function(oboset) {
    isa <-
        es_elementset(oboset) %>%
        filter(.data$is_a) %>%
        select("element", "set")
    split(isa$element, isa$set) %>%
        tibble(element = names(.)) %>%
        select(element, is_a = ".") %>%
        mutate(is_a = unname(is_a))
}

.export_obo <- function(tbl, path) {
    stopifnot(
        .is_tbl_elementset(es_elementset(tbl)),
        `'path' exists` = !file.exists(path)
    )
    
    version <-
        metadata(tbl)[["obo_header"]] %>%
        filter(.data$key == "format-version") %>%
        pull(value)
    term_tags <- .obo_term_tags(version)

    elements <- es_element(tbl)

    if (!"is_a" %in% names(elements)) {
        isa <- .oboset_is_a(tbl)
        elements <- elements %>% left_join(isa, by = "element")
    }

    if ("obsolete" %in% names(elements) & is.logical(elements$obsolete)) {
        elements <-
            elements %>%
            rename(is_obsolete = "obsolete") %>%
            mutate(is_obsolete = as.integer(.data$is_obsolete))
    }

    if (!"id" %in% names(elements))
        elements <- elements %>% rename(id = "element")

    ## order elements by id
    elements <- elements %>% arrange(.data$id)

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

    ## output
    con <- file(path, "at")

    ## write header lines
    header <- metadata(tbl)$obo_header
    writeLines(paste0(header$key, ": ", header$value), con)

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
