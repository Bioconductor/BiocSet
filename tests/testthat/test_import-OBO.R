context("import-OBO")

test_that("'.import_obo()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    ## minimal
    foo <- .import_obo(oboFile)
    expect_s4_class(foo, "OBOSet")
    expect_true(.is_tbl_elementset(.elementset(foo)))
    expect_identical(dim(.element(foo)), c(8L, 3L))
    expect_identical(dim(.set(foo)), c(8L, 2L))
    expect_identical(dim(.elementset(foo)), c(36L, 3L))

    element_nms <- c("element", "name", "obsolete")
    expect_true(all(colnames(.element(foo)) %in% element_nms))
    expect_type(.element(foo)$obsolete, "logical")

    set_nms <- c("set", "name")
    expect_true(all(colnames(.set(foo)) %in% set_nms))
    expect_type(.set(foo)$name, "character")

    elementset_nms <- c("element", "set", "is_a")
    expect_true(all(colnames(.elementset(foo)) %in% elementset_nms))
    expect_type(.elementset(foo)$is_a, "logical")
    
    ## everything
    foo_all <- .import_obo(oboFile, extract_tag = "everything")
    expect_s4_class(foo_all, "OBOSet")
    expect_true(.is_tbl_elementset(.elementset(foo_all)))
    expect_identical(dim(.element(foo_all)), c(8L, 14L))
    expect_identical(dim(.set(foo_all)), c(8L, 2L))
    expect_identical(dim(.elementset(foo_all)), c(36L, 3L))

    expect_type(.element(foo_all)$obsolete, "logical")
    expect_true(all(colnames(.set(foo_all)) %in% set_nms))
    expect_type(.set(foo_all)$name, "character")
    expect_true(all(colnames(.elementset(foo_all)) %in% elementset_nms))
    expect_type(.elementset(foo_all)$is_a, "logical")

    expect_error(.import_obo())
    expect_error(.import_obo(oboFile, all))
    expect_error(.import_obo(oboFile, extract_tag = all))
    expect_error(.import_obo(oboFile, extract_tag = "all"))
})

test_that("'import()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    ## minimal
    foo <- import(oboFile)
    expect_identical(foo, .import_obo(oboFile))

    ## everything
    foo_all <- import(oboFile, extract_tag = "everything")
    expect_identical(foo_all, .import_obo(oboFile, extract_tag = "everything"))

    expect_error(import())
    expect_error(import(oboFile, all))
    expect_error(import(oboFile, extract_tag = all))
    expect_error(import(oboFile, extract_tag = "all"))
})

test_that("'.element_elementset()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    foo <- import(oboFile)
    tst <- .element_elementset(foo)
    expect_true(is_tibble(tst))
    expect_identical(dim(tst), c(7L, 5L))
    names <- c("element", "name", "obsolete", "set", "is_a")
    expect_true(all(colnames(tst) %in% names))
    expect_true(all(tst$is_a))

    foo2 <- import(oboFile, extract_tag = "everything")
    tst2 <- .element_elementset(foo2)
    expect_identical(dim(tst2), c(7L, 16L))
})

test_that("'.set_elementset()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    foo <- import(oboFile)
    tst <- .set_elementset(foo)
    expect_true(is_tibble(tst))
    expect_identical(dim(tst), c(7L, 4L))
    names <- c("set", "name", "element", "is_a")
    expect_true(all(colnames(tst) %in% names))
    expect_true(all(tst$is_a))

    foo2 <- import(oboFile, extract_tag = "everything")
    tst2 <- .set_elementset(foo2)
    expect_identical(.set_elementset(foo2), tst)
})

test_that("'oboset_element_children()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    obo <- import(oboFile)
    tst <- oboset_element_children(obo)
    expect_false(.is_tbl_elementset(tst))
    expect_identical(dim(tst), c(7L, 2L))
    names <- c("element", "children")
    expect_true(all(colnames(tst) %in% names))
})

test_that("'oboset_element_parents()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    obo <- import(oboFile)
    tst <- oboset_element_parents(obo)
    expect_false(.is_tbl_elementset(tst))
    expect_identical(dim(tst), c(7L, 2L))
    names <- c("element", "parents")
    expect_true(all(colnames(tst) %in% names))
})

test_that("'obset_element_ancestors()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    obo <- import(oboFile)
    tst <- oboset_element_ancestors(obo)
    expect_false(.is_tbl_elementset(tst))
    expect_identical(dim(tst), c(36L, 2L))
    names <- c("element", "ancestor")
    expect_true(all(colnames(tst) %in% names))
})

test_that("'oboset_set_children()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    obo <- import(oboFile)
    tst <- oboset_set_children(obo)
    expect_false(.is_tbl_elementset(tst))
    expect_identical(dim(tst), c(7L, 2L))
    names <- c("set", "children")
    expect_true(all(colnames(tst) %in% names))
})

test_that("'oboset_set_parents()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    obo <- import(oboFile)
    tst <- oboset_set_parents(obo)
    expect_false(.is_tbl_elementset(tst))
    expect_identical(dim(tst), c(7L, 2L))
    names <- c("set", "parents")
    expect_true(all(colnames(tst) %in% names))
})

test_that("'oboset_set_ancestors()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    obo <- import(oboFile)
    tst <- oboset_set_ancestors(obo)
    expect_false(.is_tbl_elementset(tst))
    expect_identical(dim(tst), c(36L, 2L))
    names <- c("set", "ancestor")
    expect_true(all(colnames(tst) %in% names))
})

test_that("'.export_obo()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    ## minimal
    foo <- import(oboFile)
    fl <- tempfile(fileext=".obo")
    .export_obo(foo, fl)
    expect_equal(foo, import(fl))

    ## everything
    foo <- import(oboFile, extract_tag = "everything")
    fl <- tempfile(fileext=".obo")
    .export_obo(foo, fl)
    expect_equal(foo, import(fl, extract_tag = "everything"))

    foo2 <- foo %>% select_element(!obsolete)
    expect_error(.export_obo(foo2, fl))
})

test_that("'export()' works", {
    oboFile <- system.file("extdata", "sample_go.obo", package = "BiocSet")

    ## minimal
    foo <- import(oboFile)
    fl <- tempfile(fileext=".obo")
    export(foo, fl)
    expect_equal(foo, import(fl))

    ## everything
    foo <- import(oboFile, extract_tag = "everything")
    fl <- tempfile(fileext=".obo")
    export(foo, fl)
    expect_equal(foo, import(fl, extract_tag = "everything"))

    foo2 <- foo %>% select_element(!obsolete)
    expect_error(export(foo2, fl))
})
