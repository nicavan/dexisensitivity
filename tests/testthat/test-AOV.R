test_that("AOV : same output as JEB's scripts", {
    # Load the simple DEXi tree needed for the test
    Trees <- readRDS(system.file("testdata", "testtrees.rds",
                                 package = "dexiranalysis"))
    Tree <- Trees[[1]]

    # Setup a random seed for the test
    set.seed(42)

    ### - Unit test - ###
    out1 <- AOV_DEXi(Tree)
    out2 <- readRDS(system.file("testdata", "TestAOVsmall.rds",
                                package = "dexiranalysis"))

    expect_equal(out1, out2)
    ### - End - ###

    # restore random seed
    set.seed(NULL)
})
