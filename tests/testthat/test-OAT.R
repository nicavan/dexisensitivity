test_that("OAT : same output as JEB's scripts", {
    Trees <- readRDS(system.file("testdata", "testtrees.rds",
                                 package="dexiranalysis"))

    Tree <- Trees[[1]]

    set.seed(42)
    optionOAT <- createOptions(Tree, 1)

    out1 <- OAT(Tree, optionOAT)
    out2 <- readRDS(system.file("testdata", "TestOATsmall.rds",
                                package="dexiranalysis"))

    expect_equal(out1, out2)
})
