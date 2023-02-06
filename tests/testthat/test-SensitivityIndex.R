test_that("SI : same output as JEB's scripts", {
    # Load the simple DEXi tree needed for the test
    Trees <- readRDS(system.file("testdata", "testtrees.rds",
                                 package = "dexiranalysis"))
    Tree <- Trees[[1]]

    # Setup a random seed for the test
    set.seed(42)

    # Unit test
    out1 <- SI_DEXi(Tree, isFile = FALSE)

    out2 <- readRDS(system.file("testdata", "TestSIsmall.rds",
                                package = "dexiranalysis"))

    expect_equal(out1, out2)

    # restore random seed
    set.seed(NULL)
})
