check_ancestry <- function(object) {
    errors <- character()

    if (!("children" %in% names(object@set))) {
        msg <- paste("children needs to be included in the obo file.")
        errors <- c(errors, msg)
    }

    if (!("parent" %in% names(oject@set))) {
        msg <- paste("parent needs to be included in the obo file.")
        errors <- c(errors, msg)
    }

    if (!("ancestor" %in% names(object@set))) {
        msg <- paste("ancestor needs to be included in the obo file.")
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
}

setClass("OboSet", contains = "BiocSet", validity = check_ancestry)

import.obo <- function(path) {

    file <- basename(path)

    obo <- get_ontology(file, extract_tags = "everything")
    minimal <- get_ontology(file)

    elements <- 
        as_tibble(lapply(obo, as.vector)) %>%
        filter(!namespace %in% "external") %>%
        renames(element = id)

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
