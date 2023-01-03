test_that("createOption : same output as JEB's scripts", {

    PMTree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                package="dexiranalysis"))

    out1 <- createOptions(PMTree[[1]], nbOptions = 3, aSeed = 42)

    out2 <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                package="dexiranalysis"))

    expect_equal(out1, out2)
})


test_that("eval 1 option : same output as JEB's scripts", {

    PMTree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                  package="dexiranalysis"))

    options <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                   package="dexiranalysis"))

    out1 <- EvaluateScenario(PMTree[[1]], as.matrix(options[, 1]))

    out2 <- readRDS(system.file("testdata", "DEXiPMeval1option.rds",
                                package="dexiranalysis"))

    expect_equal(out1, out2)
})


test_that("eval all option : same output as JEB's scripts", {

    PMTree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                  package="dexiranalysis"))

    options <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                   package="dexiranalysis"))

    out1 <- EvaluateScenarios(PMTree[[1]], options)

    out2 <- readRDS(system.file("testdata", "DEXiPMevalalloptions.rds",
                                package="dexiranalysis"))

    out3 <- sapply(1:dim(options)[2],function(x)EvaluateScenario(PMTree[[1]],as.matrix(options[,x])))

    expect_equal(out1, out2)
    expect_equal(out1, out3)
})


test_that("Saved and loaded option are the same when option have rownames", {

    options <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                   package="dexiranalysis"))

    colnames(options) <- c("A", "B", "C")

    saveOptions(options,
                "C:/Users/rallart/E-DISC/option.csv")

    out1 <- loadOptions("C:/Users/rallart/E-DISC/option.csv")

    expect_equal(out1, options)
})


