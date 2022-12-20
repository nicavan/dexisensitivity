test_that("same output as JEB's scripts 2", {

    PMTree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                package="dexiranalysis"))

    out1 <- createOptions(PMTree[[1]], nbOptions = 3, aSeed = 42)

    out2 <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                package="dexiranalysis"))

    expect_equal(out1, out2)
})
