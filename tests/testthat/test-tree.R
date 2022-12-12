# Tests scripts for Tree class #
################################


#### - Tree class test - #### ####
test_that("Tree is S4", {
    expect_equal(sloop::otype(new(Class = "Tree")),"S4")
})



test_that("Empty tree return structured tree with 0 length S4 attributes", {
    empty_tree <- new(Class = "Tree")
    expect_equal(empty_tree@nbAttributes, new(Class = "numeric"))
    expect_equal(empty_tree@nbLeaves, new(Class = "numeric"))
    expect_equal(empty_tree@Depth, new(Class = "numeric"))
    expect_equal(empty_tree@Attributes, new(Class = "character"))
    expect_equal(empty_tree@Leaves, new(Class = "character"))
    expect_equal(empty_tree@Aggregated, new(Class = "character"))
    expect_equal(empty_tree@isMultiple, new(Class = "logical"))
    expect_equal(empty_tree@Multiple, new(Class = "data.frame"))
    expect_equal(empty_tree@isLeafAggregated, new(Class = "logical"))
    expect_equal(empty_tree@LeafAggregated, new(Class = "character"))
    expect_equal(empty_tree@Paths, new(Class = "list"))
    expect_equal(empty_tree@Nodes, new(Class = "list"))
    expect_equal(empty_tree@EvalOrder, new(Class = "numeric"))
    expect_equal(empty_tree@rootName, new(Class = "character"))
})



#### - print method test - #### ####
test_that("Empty Tree print correctly", {
    empty_tree <- new(Class = "Tree")

    empty_print <- capture.output({
        cat("Root name: ")
        cat("\nNumber of attributes: 0")
        cat("\nNumber of aggregated attributes: 0")
        cat("\nNumber of true leaves (no multiple, no aggregated): ")
        cat("\nMaximum depth: ")
        cat("\nList of repeated aggregated nodes: Non")
        cat("\nNo multiple leaves")
        cat("\nNo Leaf-Aggregated Leaf")
    })

    print(empty_print)

    expect_equal(capture.output(print(empty_tree)), empty_print)
})

#### - show method test - #### ####
test_that("Empty Tree show correct message", {
    expect_equal(capture.output(new(Class = "Tree")),
                 "*** Tree without attributes ***")
})


#### - describe method test - #### ####
test_that("describe return correct error if no Nodes", {
    empty_tree <- new(Class = "Tree")

    expect_error(describe(empty_tree),
                 "Tree without any node!")
})

test_that("Empty Tree describe correct message", {

    PMtest <- readRDS(system.file("testdata", "TestMTDEXiPM.rds",
                                  package="dexiranalysis"))
    out1 <- createTree(XML::xmlDeserializeHook(PMtest))

    out <- readRDS(system.file("testdata", "describeDEXiPM.rds",
                               package="dexiranalysis"))


    expect_equal(capture.output(describe(out1[[1]])),
                 out)
})

#### - - #### ####
# Pour voir ce que fait un test qui fail ou avec error
# test_that("Tree is S4", {
#     expect_equal(otype(),"S4")
# })

