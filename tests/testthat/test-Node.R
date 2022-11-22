# Tests scripts for Node class #
################################

test_that("Node is S4", {
    expect_equal(sloop::otype(new(Class = "Node")),"S4")
})


test_that("Empty Node return structured Node with 0 length S4 attributes", {
    empty_Node <- new(Class = "Node")
    expect_equal(empty_Node@id, new(Class = "numeric"))
    expect_equal(empty_Node@name, new(Class = "character"))
    expect_equal(empty_Node@isLeaf, new(Class = "logical"))
    expect_equal(empty_Node@isLeafAndAggregated, new(Class = "logical"))
    expect_equal(empty_Node@children, new(Class = "character"))
    expect_equal(empty_Node@sisters, new(Class = "character"))
    expect_equal(empty_Node@mother, new(Class = "character"))
    expect_equal(empty_Node@aggregation, new(Class = "matrix"))
    expect_equal(empty_Node@Proba, new(Class = "numeric"))
    expect_equal(empty_Node@Depth, new(Class = "numeric"))
    expect_equal(empty_Node@Twin, new(Class = "numeric"))
    expect_equal(empty_Node@CondiProbaList, new(Class = "list"))
    expect_equal(empty_Node@rangeScale, new(Class = "numeric"))
    expect_equal(empty_Node@scaleLabel, new(Class = "character"))
    expect_equal(empty_Node@nodePath, new(Class = "character"))
})
