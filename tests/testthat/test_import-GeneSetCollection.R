context("import-GeneSetCollection")

test_that("'GeneSetCollection_from_BiocSet()' works", {
    es <- BiocSet(set1 = letters[3:9], set2 = LETTERS[13:23])
    gsc <- GeneSetCollection_from_BiocSet(es)

    expect_s4_class(gsc, "GeneSetCollection")
    expect_identical(length(names(gsc)), 2L)
})

test_that("'BiocSet_from_GeneSetCollection()' works", {
    es <- BiocSet(set1 = letters[2:15], set2 = LETTERS[4:13])
    gsc <- GeneSetCollection_from_BiocSet(es)
    es2 <- BiocSet_from_GeneSetCollection(gsc)

    expect_s4_class(es2, "BiocSet")
    expect_identical(es2, es)
})
