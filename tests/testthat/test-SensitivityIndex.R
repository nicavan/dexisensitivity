test_that("SI : same output as JEB's scripts", {
    Trees <- readRDS(system.file("testdata", "testtrees.rds",
                                 package="dexiranalysis"))

    Tree <- Trees[[1]]

    set.seed(42)
    out1 <- SI_DEXi(Tree, isFile = FALSE)

    out2 <- readRDS(system.file("testdata", "TestSIsmall.rds",
                                package="dexiranalysis"))

    expect_equal(out1, out2)
})
