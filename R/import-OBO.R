.OBOFile = setClass("OBOFile", contains = "RTLFile")

OBOFile = function(resource, ...)
    .OBOFile(resource = resource)

#' @importFrom ontologyIndex get_ontology
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr rename anti_join
.import_obo <- function(path, extract_tags = "minimal") {
    stopifnot(extract_tags %in% c("minimal", "everything"))
    
    ## Need to account for diff format versions, order and tags can change
    ## These tags are for format version 1.2
    term_tags <- c(
        "id", "is_anonymous", "name", "namespace", "alt_id", "def",
        "comment", "subset", "synonym", "xref", "is_a", "intersection_of",
        "union_of", "disjoint_from", "relationship", "is_obsolete",
        "replaced_by", "consider", "created_by", "creation_date"
    )    
    if (extract_tags == "everything") {
        obo <- get_ontology(path, extract_tags = "everything")
        elements <- as_tibble(lapply(obo, as.vector)) %>%
            filter(!.data$namespace %in% "external") %>%
            rename(element = "id")
        info_columns <- elements[-c(1:6)]
        if (!all(names(info_columns) %in% term_tags)) {
            keep <- names(elements)[names(elements) %in% term_tags]
            drop <- names(info_columns)[!names(info_columns) %in% term_tags]
            elements <- elements %>% select(1:6, keep)
            cat("The columns:\n", 
                drop, 
                "\nhave been dropped from es_element.\n",
                sep = "\n"
            )
        }
    } else {
        obo <- get_ontology(path)
        elements <- as_tibble(lapply(obo, as.vector)) %>%
            filter(grepl("GO:", .data$id, fixed = TRUE)) %>%
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

    ## header
    version <- attr(obo, "version")
    pattern <- "^([^:]+):(.*)"
    obo_header <- tibble(
        key = sub(pattern, "\\1", version),
        value = trimws(sub(pattern, "\\2", version))
    )
    metadata <- list(obo_header = obo_header)

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

    ## order elements by id
    elements <- elements %>% arrange(.data$id)

    ## Need to account for diff format versions, order and tags can change
    ## These tags are for format version 1.2
    term_tags <- c(
        "id", "is_anonymous", "name", "namespace", "alt_id", "def",
        "comment", "subset", "synonym", "xref", "is_a", "intersection_of",
        "union_of", "disjoint_from", "relationship", "is_obsolete",
        "replaced_by", "consider", "created_by", "creation_date"
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
