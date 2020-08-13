## This code shows how the sample_go.obo was created

## Start by downloading the LARGE go.obo file from GeneOntology
download.file("http://current.geneontology.org/ontology/go.obo", "obo_file.obo")

## Then use the import function to read in the obo file and create an OBOSet 
## object which is an extension of the BiocSet class
foo <- import("obo_file.obo", extract_tag = "everything")

## From here we created a list of ancestors to create a smaller obo file with 
## proper relationships
small_tst <- es_element(foo)[1,] %>%
    unnest("ancestors") %>%
    select("element", "ancestors") %>%
    unlist() %>%
    unique()

small_oboset <- foo %>% filter_elementset(element %in% small_tst)

## We demonstrate how to export to a temporary file but
## we exported the OBOset to the sample_go.obo file in inst/extdata of BiocSet
fl <- tempfile(fileext=".obo")
export(small_oboset, fl)
