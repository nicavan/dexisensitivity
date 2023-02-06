test_that("MC : same output as JEB's scripts", {
    # Load the simple DEXi tree needed for the test
    Trees <- readRDS(system.file("testdata", "testtrees.rds",
                                    package = "dexiranalysis"))
    Tree <- Trees[[1]]

    # Setup a random seed for the test
    set.seed(42)

    ### - Unit test - ###
    out1 <- MonteCarlo(Tree, 1000,
                       verbose = F)
    out2 <- readRDS(system.file("testdata", "TestMCsmall.rds",
                                package = "dexiranalysis"))

    expect_equal(out1, out2)
    ### - End - ###

    # restore random seed
    set.seed(NULL)
})
