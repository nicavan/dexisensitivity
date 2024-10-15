#### create_options test ####
test_that("create_options : same output as previous version with masc2", {
  # Load the complex DEXi tree needed for the test
  masc2 <- dexisensitivity::masc2

  # Unit test
  test_output <- create_options(masc2, num_options = 3, seed = 42)

  expected_output <- readRDS(system.file("testdata",
    "test_create_options_masc2.rds",
    package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)
})

#### evaluate_scenario test ####
test_that("evaluate_scenario : same output as previous version with masc2", {
  # Load the complex DEXi tree needed for the test
  masc2 <- dexisensitivity::masc2

  options <- readRDS(system.file("testdata", "test_create_options_masc2.rds",
    package = "dexisensitivity"
  ))

  # Unit test
  test_output <- evaluate_scenario(masc2, as.matrix(options[, 1]))

  expected_output <- readRDS(system.file("testdata", "masc2_eval1option.rds",
    package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)
})

test_that("evaluate_scenarios : same output as previous version with masc2", {
  # Load the complex DEXi tree and option needed for the test
  masc2 <- dexisensitivity::masc2

  options <- readRDS(system.file("testdata", "test_create_options_masc2.rds",
    package = "dexisensitivity"
  ))

  # Unit test
  test_output <- evaluate_scenarios(masc2, options)

  expected_output_a <- readRDS(system.file("testdata",
    "masc2_evalalloptions.rds",
    package = "dexisensitivity"
  ))

  expected_output_b <- 1:dim(options)[2] |>
    sapply(function(x) {
      evaluate_scenario(masc2, as.matrix(options[, x]))
    })

  expect_equal(test_output, expected_output_a)
  expect_equal(test_output, expected_output_b)
})

#### show and compare scenario test ####
test_that("show_scenario : same output as previous version with masc2", {
  skip_if_not_installed("vdiffr")
  # Load complex DEXi tree and scenarios needed for the test
  Scenario <- readRDS(system.file("testdata", "masc2_evalalloptions.rds",
    package = "dexisensitivity"
  ))
  masc2 <- dexisensitivity::masc2

  # Unit test
  vdiffr::expect_doppelganger(
    "showscenario-plot",
    show_scenario(as.matrix(Scenario[, 1]),
      tree = masc2,
      label_y = T
    )
  )

  expect_equal(par()$cex, 1)
  expect_equal(par()$mgp, c(3, 1, 0))
  expect_equal(par()$oma, c(0, 0, 0, 0))
})

test_that("compare_scenarios : same output as previous version with masc2", {
  skip_if_not_installed("vdiffr")
  # Load complex DEXi tree and scenarios needed for the test
  Scenario <- readRDS(system.file("testdata", "masc2_evalalloptions.rds",
    package = "dexisensitivity"
  ))
  masc2 <- dexisensitivity::masc2

  # Unit test
  plot_compareScenario <- compare_scenarios(
    masc2, Scenario,
    c(
      "Contribution au developpement durable",
      "Dimension economique",
      "Dimension sociale",
      "Dimension environnementale"
    )
  )

  vdiffr::expect_doppelganger("compareScenario-plot", plot_compareScenario)

  expect_equal(par()$ps, 12)
})
