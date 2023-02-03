test_that("AOV : same output as JEB's scripts", {
    Trees <- readRDS(system.file("testdata", "testtrees.rds",
                                 package="dexiranalysis"))

    Tree <- Trees[[1]]

    set.seed(42)
    out1 <- AOV_DEXi(Tree)
    out2 <- readRDS(system.file("testdata", "TestAOVsmall.rds",
                                package="dexiranalysis"))

    expect_equal(out1, out2)
})
