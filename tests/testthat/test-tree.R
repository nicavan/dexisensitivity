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
        cat("\nList of repeated aggregated nodes:",
            if(length(which(table(empty_tree@Aggregated) > 1))) {
                names(which(table(empty_tree@Aggregated)>1))
            } else {"Non"}
        )
        if(length(empty_tree@isMultiple) > 0 && empty_tree@isMultiple) {
            cat("\nMultiple leaves: \n")
            print(empty_tree@Multiple)
        } else {cat("\nNo multiple leaves")}
        if(length(empty_tree@isLeafAggregated) > 0 &&
           empty_tree@isLeafAggregated) {
            cat("\nLeaf-Aggregated attributes: \n")
            print(empty_tree@LeafAggregated)
        } else {cat("\nNo Leaf-Aggregated Leaf")}
    })

    print(empty_print)

    expect_equal(capture.output(print(empty_tree)), empty_print)
})

#### - show method test - #### ####
test_that("Empty Tree show correct message", {
    expect_equal(capture.output(new(Class = "Tree")),
                 "*** Tree without attributes ***")
})


#### - - #### ####
# Pour voir ce que fait un test qui fail ou avec error
# test_that("Tree is S4", {
#     expect_equal(otype(),"S4")
# })

