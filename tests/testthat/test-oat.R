test_that("OAT : same output as previous version with small tree", {
  # Load the simple DEXi tree needed for the test
  list_tree <- readRDS(system.file("testdata", "testtrees.rds",
    package = "dexiranalysis"
  ))
  tree <- list_tree[[1]]

  # Setup a random seed for the test
  set.seed(42)

  ### - Unit test - ###
  optionOAT <- create_options(tree, 1)

  test_output <- oat(tree, optionOAT)
  expected_output <- readRDS(system.file("testdata", "test_oat_small.rds",
    package = "dexiranalysis"
  ))
  expect_equal(test_output, expected_output)
  ### - End - ###

  # restore random seed
  set.seed(NULL)
})
