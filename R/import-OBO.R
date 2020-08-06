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
#' @importFrom dplyr rename anti_join right_join
.import_obo <- function(path, extract_tags = "minimal") {
    stopifnot(
        extract_tags %in% c("minimal", "everything")
    )
    
    obo <- get_ontology(path, extract_tags = "everything")

    key <- NULL
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
    is_a <- NULL
    inner_join(es_element(oboset),
        filter(es_elementset(oboset), is_a), 
        by = "element"
    )
}

.set_elementset <- function(oboset) {
    inner_join(es_set(oboset), es_elementset(oboset), by = "set")
}

#' Functions to display relationships of an \code{OBOSet} object
#' @rdname obo_relations
#' @name obo_relations
#' @description These functions will display the relationships (children, 
#'     parents, or ancestors) for either the elements or the sets of an 
#'     \code{OBOSet} object.
#' @param oboset The \code{OBOSet} of interest.
#' @importFrom tidyr unnest
#' @importFrom dplyr inner_join
#' @return A 2 column tibble.
#' @export
#' @examples
#' oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")
#' obo <- import(oboFile)
#' oboset_element_children(obo) 
oboset_element_children <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    .element_elementset(oboset) %>%
        select("set", "element") %>%
        rename(element = "set", children = "element")
}

#' @rdname obo_relations
#' @export
#' @examples
#' 
#' oboset_element_parents(obo)
oboset_element_parents <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    .element_elementset(oboset) %>%
        select("element", "set") %>%
        rename(parents = "set")
}

#' @rdname obo_relations
#' @importFrom stats setNames
#' @export
#' @examples
#'
#' oboset_element_ancestors(obo)
oboset_element_ancestors <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    element <- NULL
    oboset %>%
        filter_element(element %in% es_elementset(oboset)$element) %>%
        .ancestors
}

#' @rdname obo_relations
#' @export
#' @examples
#'
#' oboset_set_children(obo)
oboset_set_children <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    .set_elementset(oboset) %>%
        select("set", "element") %>%
        rename(children = "element")
}

#' @rdname obo_relations
#' @export
#' @examples
#'
#' oboset_set_parents(obo)
oboset_set_parents <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    .set_elementset(oboset) %>%
        select("element", "set") %>%
        rename(set = "element", parents = "set")
}

#' @rdname obo_relations
#' @export
#' @examples
#'
#' oboset_set_ancestors(obo)
oboset_set_ancestors <- function(oboset) {
    stopifnot(is(oboset, "OBOSet"))
    set <- NULL
    oboset %>%
        filter_set(set %in% es_elementset(oboset)$set) %>%
        .ancestors
}

.ancestors_merge <-
    function(ancestors, child, parent)
{
    ## ancestors of each child: the parent, and the parent's
    ## ancestors. A child is it's own ancestor
    ancestors0 <- Map(c, child, parent, ancestors[parent])
    ## a child may have more than one parent; unlist and re-split by child
    ancestors1 <- split(
        unlist(ancestors0, use.names = FALSE),
        rep(child, lengths(ancestors0))
    )
    
    ## combine newly identified ancestors with existing ancestors
    ## some newly identified ancestors may add to existing ancestors
    UFUN <- function(...) unique(c(...))
    duplicates <- intersect(names(ancestors), names(ancestors1))
    c(
        ancestors[setdiff(names(ancestors), duplicates)],
        ancestors1[setdiff(names(ancestors1), duplicates)],
        Map(UFUN, ancestors[duplicates], ancestors1[duplicates])
    )
}

.ancestors <-
    function(obo)
{
    stopifnot(is(obo, "OBOSet"))
    
    is_a <- NULL
    es <- es_elementset(obo) %>%
        filter(is_a) %>%
        select("element", "set") %>%
        distinct()
    
    child <- es %>% pull("element")
    parent <- es %>% pull("set")
    
    ancestor0 <- parent[!parent %in% child]
    root <- ancestor <- unique(ancestor0)
    ancestors <- setNames(as.list(root), root)
    
    total <- length(ancestor0)
    while (length(ancestor)) {
        ## index of parents who will beccome an ancestor this iteration
        idx <- parent %in% ancestor
        
        ## update list of ancestors
        ancestors <- .ancestors_merge(ancestors, child[idx], parent[idx])
        
        ## remove child-parent pairs of ancestors
        child <- child[!idx]
        parent <- parent[!idx]
        ancestor0 <- parent[!parent %in% child]
        ancestor <- unique(ancestor0)
    }
    
    ## all elements are there own ancestor
    child <-
        es_elementset(obo) %>%
        pull("element") %>%
        unique()
    ancestors <- .ancestors_merge(ancestors, child, child)
    
    ## tidy up
    ancestors <- lapply(ancestors, unique)
    tibble(
        element = rep(names(ancestors), lengths(ancestors)),
        ancestor = unlist(ancestors, use.names = FALSE)
    )
}

.oboset_is_a <- function(oboset) {
    . <- element <- is_a <- NULL
    isa <-
        es_elementset(oboset) %>%
        filter(.data$is_a) %>%
        select("element", "set")
    split(isa$set, isa$element) %>%
        tibble(element = names(.)) %>%
        select(element, is_a = ".") %>%
        mutate(is_a = unname(is_a))
}

.export_obo <- function(tbl, path) {
    stopifnot(
        .is_tbl_elementset(es_elementset(tbl)),
        `'path' exists` = !file.exists(path)
    )
   
    value <- NULL
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
