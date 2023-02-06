# Tests scripts for Node class #
################################

#### - General tests - #### ####
test_that("Node is S4", {
    expect_equal(sloop::otype(new(Class = "Node")), "S4")
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


#### - print method test - #### ####
test_that("Empty Node print correctly", {
    empty_node <- new(Class = "Node")

    empty_print <- capture.output({
        cat("Node name: ")
        cat("\nID: ")
        cat("\nNode depth: ")
        cat("\nFrom root to node: ->")
        cat("\nIs it a leaf: ")
        cat("\nIs is a leaf-aggregated: FALSE")
        cat("\nMother: ")
        cat("\nSisters: None")
        cat("\nChildren: None")
        cat("\nEstimated weights: ")
    })

    expect_equal(capture.output(print(empty_node)), empty_print)

})


test_that("compareScenario : same output as JEB's scripts and par is reset", {
    # Load the complex DEXi tree needed for the test
    lDEXi <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                 package = "dexiranalysis"))
    DEXi <- lDEXi[[1]]

    # Unit test
    expect_equal(getEstimatedWeights(DEXi@Nodes[[1]]),
                 c("xECONOMIC" = 1/3,
                   "xSOCIAL" = 1/3,
                   "xENVIRONMENTAL" = 1/3))
})



test_that("createAggregationMatrix : same output as JEB's scripts and par is reset", {
    # Load the complex DEXi tree needed for the test
    lDEXi <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                 package = "dexiranalysis"))
    DEXi <- lDEXi[[1]]


    myWeights <- c(0.2, 0.2, 0.6)

    # Setup a random seed for the test
    set.seed(42)

    ### - Unit test - ###
    newAggregation <- createAggregationMatrix(DEXi@Nodes[[1]], myWeights, 5)

    out <- readRDS(system.file("testdata", "newAggregation42DEXiPM.rds",
                               package = "dexiranalysis"))

    expect_equal(newAggregation, out)
    ### - End - ###

    # restore random seed
    set.seed(NULL)
})
