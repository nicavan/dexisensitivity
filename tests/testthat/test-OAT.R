test_that("OAT : same output as JEB's scripts", {
    # Load the simple DEXi tree needed for the test
    list_tree <- readRDS(system.file("testdata", "testtrees.rds",
                                     package = "dexiranalysis"))
    tree <- list_tree[[1]] %>% convertTreeClass()

    # Setup a random seed for the test
    set.seed(42)

    ### - Unit test - ###
    optionOAT <- createOptions(tree, 1)

    test_output <- OAT(tree, optionOAT)
    expected_output <- readRDS(system.file("testdata", "TestOATsmall.rds",
                                           package = "dexiranalysis"))
    expect_equal(test_output, expected_output)
    ### - End - ###

    # restore random seed
    set.seed(NULL)
})
