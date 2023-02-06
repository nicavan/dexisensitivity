test_that("OAT : same output as JEB's scripts", {
    # Load the simple DEXi tree needed for the test
    Trees <- readRDS(system.file("testdata", "testtrees.rds",
                                 package = "dexiranalysis"))
    Tree <- Trees[[1]]

    # Setup a random seed for the test
    set.seed(42)

    ### - Unit test - ###
    optionOAT <- createOptions(Tree, 1)

    out1 <- OAT(Tree, optionOAT)
    out2 <- readRDS(system.file("testdata", "TestOATsmall.rds",
                                package = "dexiranalysis"))
    expect_equal(out1, out2)
    ### - End - ###

    # restore random seed
    set.seed(NULL)
})
