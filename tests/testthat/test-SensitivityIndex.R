test_that("SI : same output as JEB's scripts", {
    # Load the simple DEXi tree needed for the test
    trees <- readRDS(system.file("testdata", "testtrees.rds",
                                 package = "dexiranalysis"))
    tree <- trees[[1]]

    # Setup a random seed for the test
    set.seed(42)

    # Unit test
    test_output <- SI_DEXi(tree, isFile = FALSE)

    expected_output <- readRDS(system.file("testdata", "TestSIsmall.rds",
                                           package = "dexiranalysis"))

    expect_equal(test_output, expected_output)

    # restore random seed
    set.seed(NULL)
})
