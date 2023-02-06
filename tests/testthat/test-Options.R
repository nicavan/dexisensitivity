test_that("createOption : same output as JEB's scripts", {
    # Load the complex DEXi tree needed for the test
    PMTree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                package = "dexiranalysis"))

    # Unit test
    out1 <- createOptions(PMTree[[1]], nbOptions = 3, aSeed = 42)

    out2 <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                package = "dexiranalysis"))

    expect_equal(out1, out2)
})


test_that("eval 1 option : same output as JEB's scripts", {
    # Load the complex DEXi tree and option needed for the test
    PMTree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                  package = "dexiranalysis"))
    options <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                   package = "dexiranalysis"))

    # Unit test
    out1 <- EvaluateScenario(PMTree[[1]], as.matrix(options[, 1]))

    out2 <- readRDS(system.file("testdata", "DEXiPMeval1option.rds",
                                package = "dexiranalysis"))

    expect_equal(out1, out2)
})


test_that("eval all option : same output as JEB's scripts", {
    # Load the complex DEXi tree and option needed for the test
    PMTree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                  package = "dexiranalysis"))
    options <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                   package = "dexiranalysis"))

    # Unit test
    out1 <- EvaluateScenarios(PMTree[[1]], options)

    out2 <- readRDS(system.file("testdata", "DEXiPMevalalloptions.rds",
                                package = "dexiranalysis"))

    out3 <- 1:dim(options)[2] %>%
        sapply(function(x) {
            EvaluateScenario(PMTree[[1]], as.matrix(options[, x]))
        })

    expect_equal(out1, out2)
    expect_equal(out1, out3)
})


test_that("Saved and loaded option are the same when option have rownames", {
    # Load the complex DEXi option needed for the test
    options <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                   package = "dexiranalysis"))

    # Unit test
    colnames(options) <- c("A", "B", "C")
    saveOptions(options, "C:/Users/rallart/E-DISC/option.csv")

    out1 <- loadOptions("C:/Users/rallart/E-DISC/option.csv")

    expect_equal(out1, options)
})


test_that("saveScenarios well save a file", {
    # Load scenarios needed for the test
    Scenario <- readRDS(system.file("testdata", "TestscenariosAll.rds",
                                   package = "dexiranalysis"))

    # Unit test
    saveScenarios(Scenario, "C:/Users/rallart/E-DISC/Scenario.csv")

    expect_equal(file.exists("C:/Users/rallart/E-DISC/Scenario.csv"), T)
})



test_that("showScenario : same output as JEB's scripts and par is reset", {
    # Load complex DEXi tree and scenarios needed for the test
    Scenario <- readRDS(system.file("testdata", "TestscenariosAll.rds",
                                    package = "dexiranalysis"))
    lDEXi <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                 package = "dexiranalysis"))
    DEXi <- lDEXi[[1]]

    # Unit test
    vdiffr::expect_doppelganger("showScenario-plot",
                                showScenario(as.matrix(Scenario[, 1]),
                                             aTree = DEXi,
                                             isLabelY = T))

    expect_equal(par()$cex, 1)
    expect_equal(par()$mgp, c(3, 1, 0))
    expect_equal(par()$oma, c(0, 0, 0, 0))
})


test_that("compareScenario : same output as JEB's scripts and par is reset", {
    # Load complex DEXi tree and scenarios needed for the test
    Scenario <- readRDS(system.file("testdata", "TestscenariosAll.rds",
                                    package = "dexiranalysis"))
    lDEXi <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                 package = "dexiranalysis"))
    DEXi <- lDEXi[[1]]

    # Unit test
    plot_compareScenario <- compareScenario(DEXi, Scenario,
                                            c("OVERALL SUSTAINABILITY",
                                              "ECONOMIC",
                                              "SOCIAL",
                                              "ENVIRONMENTAL"))

    vdiffr::expect_doppelganger("compareScenario-plot", plot_compareScenario)

    expect_equal(par()$ps, 12)
})
