test_that("AOV : same output as previous version with small tree", {
  # Load the simple DEXi tree needed for the test
  list_tree <- readRDS(system.file("testdata", "testtrees.rds",
    package = "dexiranalysis"
  ))
  tree <- list_tree[[1]]

  # Setup a random seed for the test
  set.seed(42)

  ### - Unit test - ###
  test_output <- aov_tree(tree)
  sorted_test_output <- lapply(test_output, function(df){
    df[order(rownames(df)),]
  })

  expected_output <- readRDS(system.file("testdata", "test_aov_small.rds",
    package = "dexiranalysis"
  ))
  sorted_expected_output <- lapply(expected_output, function(df){
    df[order(rownames(df)),]
  })

  expect_equal(sorted_test_output, sorted_expected_output)
  ### - End - ###

  # restore random seed
  set.seed(NULL)
})
