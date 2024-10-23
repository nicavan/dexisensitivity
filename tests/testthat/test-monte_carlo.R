test_that("Monte Carlo : same output as previous version with small tree", {
  # Load the simple DEXi tree needed for the test
  list_tree <- readRDS(system.file("testdata", "testtrees.rds",
    package = "dexisensitivity"
  ))
  tree <- list_tree[[1]]

  # Setup a random seed for the test
  set.seed(42)

  ### - Unit test - ###
  test_output <- monte_carlo(tree, 1000)
  expected_output <- readRDS(system.file("testdata", "test_monte_carlo_small.rds",
    package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)
  ### - End - ###

  # restore random seed
  set.seed(NULL)
})
