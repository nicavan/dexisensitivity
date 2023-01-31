test_that("MC : same output as JEB's scripts", {
    Trees <- readRDS(system.file("testdata", "testtrees.rds",
                                    package="dexiranalysis"))

    Tree <- Trees[[1]]

    set.seed(42)
    out1 <- MonteCarlo(Tree, 1000,
                       verbose = F)

    out2 <- readRDS(system.file("testdata", "TestMCsmall.rds",
                                package="dexiranalysis"))

    expect_equal(out1, out2)
})
