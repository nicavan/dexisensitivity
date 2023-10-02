test_that("MC : same output as JEB's scripts", {
    # Load the simple DEXi tree needed for the test
    list_tree <- readRDS(system.file("testdata", "testtrees.rds",
                                     package = "dexiranalysis"))
    tree <- list_tree[[1]]

    # Setup a random seed for the test
    set.seed(42)

    ### - Unit test - ###
    test_output <- monte_carlo(tree, 1000,
                               verbose = F)
    expected_output <- readRDS(system.file("testdata", "TestMCsmall.rds",
                                           package = "dexiranalysis"))

    expect_equal(test_output, expected_output)
    ### - End - ###

    # restore random seed
    set.seed(NULL)
})
