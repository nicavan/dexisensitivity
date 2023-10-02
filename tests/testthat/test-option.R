test_that("createOption : same output as JEB's scripts", {
  # Load the complex DEXi tree needed for the test
  list_tree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                   package = "dexiranalysis"))
  tree <- list_tree[[1]]

  # Unit test
  test_output <- create_options(tree, num_options = 3, seed = 42)

  expected_output <- readRDS(system.file("testdata",
                                         "TestcreateOptionsDEXiPM.rds",
                                         package = "dexiranalysis"))

  expect_equal(test_output, expected_output)
})


test_that("eval 1 option : same output as JEB's scripts", {
  # Load the complex DEXi tree and option needed for the test
  list_tree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                   package = "dexiranalysis"))
  tree <- list_tree[[1]]
  options <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                 package = "dexiranalysis"))

  # Unit test
  test_output <- evaluate_scenario(tree, as.matrix(options[, 1]))

  expected_output <- readRDS(system.file("testdata", "DEXiPMeval1option.rds",
                                         package = "dexiranalysis"))

  expect_equal(test_output, expected_output)
})


test_that("eval all option : same output as JEB's scripts", {
  # Load the complex DEXi tree and option needed for the test
  list_tree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                   package = "dexiranalysis"))
  tree <- list_tree[[1]]
  options <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                 package = "dexiranalysis"))

  # Unit test
  test_output <- evaluate_scenarios(tree, options)

  expected_output_a <- readRDS(system.file("testdata",
                                           "DEXiPMevalalloptions.rds",
                                           package = "dexiranalysis"))

  expected_output_b <- 1:dim(options)[2] %>%
    sapply(function(x) {
      evaluate_scenario(tree, as.matrix(options[, x]))
    })

  expect_equal(test_output, expected_output_a)
  expect_equal(test_output, expected_output_b)
})


test_that("Saved and loaded option are the same when option have rownames", {
  # Load the complex DEXi option needed for the test
  options <- readRDS(system.file("testdata", "TestcreateOptionsDEXiPM.rds",
                                 package = "dexiranalysis"))

  # Unit test
  colnames(options) <- c("A", "B", "C")
  save_options(options, "C:/Users/rallart/E-DISC/option.csv")

  test_output <- load_options("C:/Users/rallart/E-DISC/option.csv")
  expected_output <- options

  expect_equal(test_output, expected_output)
})


test_that("save_scenarios well save a file", {
  # Load scenarios needed for the test
  Scenario <- readRDS(system.file("testdata", "TestscenariosAll.rds",
                                  package = "dexiranalysis"))

  # Unit test
  save_scenarios(Scenario, "C:/Users/rallart/E-DISC/Scenario.csv")

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
                              show_scenario(as.matrix(Scenario[, 1]),
                                            tree = DEXi,
                                            label_y = T))

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
  plot_compareScenario <- compare_scenarios(DEXi, Scenario,
                                            c("OVERALL SUSTAINABILITY",
                                              "ECONOMIC",
                                              "SOCIAL",
                                              "ENVIRONMENTAL"))

  vdiffr::expect_doppelganger("compareScenario-plot", plot_compareScenario)

  expect_equal(par()$ps, 12)
})
