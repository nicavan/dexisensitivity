# Tests scripts for Node class #
################################

#### - General tests - #### ####
test_that("Node is S4", {
    expect_equal(sloop::otype(new(Class = "Node")), "S4")
})


test_that("Empty Node return structured Node with 0 length S4 attributes", {
    empty_Node <- new(Class = "Node")
    expect_equal(empty_Node@Id, new(Class = "numeric"))
    expect_equal(empty_Node@Name, new(Class = "character"))
    expect_equal(empty_Node@IsLeaf, new(Class = "logical"))
    expect_equal(empty_Node@IsLeafAndAggregated, new(Class = "logical"))
    expect_equal(empty_Node@Children, new(Class = "character"))
    expect_equal(empty_Node@Sisters, new(Class = "character"))
    expect_equal(empty_Node@Mother, new(Class = "character"))
    expect_equal(empty_Node@Aggregation, new(Class = "matrix"))
    expect_equal(empty_Node@Probability, new(Class = "numeric"))
    expect_equal(empty_Node@Depth, new(Class = "numeric"))
    expect_equal(empty_Node@Twin, new(Class = "numeric"))
    expect_equal(empty_Node@ConditionalProbabilityList, new(Class = "list"))
    expect_equal(empty_Node@RangeScale, new(Class = "numeric"))
    expect_equal(empty_Node@ScaleLabel, new(Class = "character"))
    expect_equal(empty_Node@NodePath, new(Class = "character"))
})


#### - print method test - #### ####
test_that("Empty Node print correctly", {
    empty_node <- new(Class = "Node")

    empty_print <- capture.output({
        cat("Node name: ")
        cat("\nID: ")
        cat("\nNode depth: ")
        cat("\nFrom root to node: \n  ")
        cat("\nIs it a leaf: ")
        cat("\nIs is a leaf-aggregated: FALSE")
        cat("\nMother: ")
        cat("\nSisters: None")
        cat("\nChildren: None")
        cat("\nEstimated weights: ")
    })

    expect_equal(capture.output(print(empty_node)), empty_print)

})


test_that("get_estimated_weights : same output as JEB's scripts and par is reset", {
    # Load the complex DEXi tree needed for the test
    lDEXi <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                 package = "dexiranalysis"))
    DEXi <- lDEXi[[1]]

    # Unit test
    expect_equal(get_estimated_weights(DEXi@Nodes[[1]]),
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
    test_output <- create_aggregation_matrix(DEXi@Nodes[[1]], myWeights, 5)

    expected_output <- readRDS(system.file("testdata",
                                           "newAggregation42DEXiPM.rds",
                                           package = "dexiranalysis"))

    expect_equal(test_output, expected_output)
    ### - End - ###

    # restore random seed
    set.seed(NULL)
})

