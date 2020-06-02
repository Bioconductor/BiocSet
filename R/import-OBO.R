check_ancestry <- function(object) {
    errors <- character()

    if (!("children" %in% names(.set(object)))) {
        msg <- paste("children needs to be included in the obo file.")
        errors <- c(errors, msg)
    }

    if (!("parent" %in% names(.set(object)))) {
        msg <- paste("parent needs to be included in the obo file.")
        errors <- c(errors, msg)
    }

    if (!("ancestor" %in% names(.set(object)))) {
        msg <- paste("ancestor needs to be included in the obo file.")
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
}

setClass("OboSet", contains = "BiocSet", validity = check_ancestry)

#' @importFrom ontologyIndex get_ontology
#' @importFrom tibble as_tibble tibble
#' @importFrom plyr rename
import.obo <- function(path) {
    ## should there be an argument to indicate if 
    ## everything should be extracted or just the minimal one
    obo <- get_ontology(path, extract_tags = "everything")
    minimal <- get_ontology(path)

    elements <- 
        as_tibble(lapply(obo, as.vector)) %>%
        filter(!namespace %in% "external") %>%
        rename(element = id)

    elements_minimal <- as_tibble(lapply(minimal, as.vector))

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
    oboset
}

element_children <- function(biocset_obo) {
    biocset_obo %>% es_element() %>% select(c("element", "children"))
}

element_parents <- function(biocset_obo) {
    biocset_obo %>% es_element() %>% select(c("element", "parents"))
}

element_ancestors <- function(biocset_obo) {
    biocset_obo %>% es_element() %>% select(c("element", "ancestors"))
}

set_children <- function(biocset_obo) {
    biocset_obo %>% es_set() %>% select(c("set", "children"))
}

set_parents <- function(biocset_obo) {
    biocset_obo %>% es_set() %>% select(c("set", "parents"))
}

set_ancestors <- function(biocset_obo) {
    biocset_obo %>% es_set() %>% select(c("set", "ancestors"))
}
