## This code shows how the sample_go.obo was created

## Start by downloading the LARGE go.obo file from GeneOntology
download.file("http://current.geneontology.org/ontology/go.obo", "obo_file.obo")

## Then use the import function to read in the obo file and create an OBOSet 
## object which is an extension of the BiocSet class
foo <- import("obo_file.obo")

## From here we created a list of ancestors to create a smaller obo file with 
## proper relationships
tst_ann <- oboset_element_ancestors(foo) %>%
    head() %>%
    unnset("ancestors") %>%
    unlist() %>%
    unique()

tst_ann

## A smaller obo file was created by searching for these terms in the original 
## obo file
## We also added in the parent term GO:0000002 so that the relationships would 
## be complete
