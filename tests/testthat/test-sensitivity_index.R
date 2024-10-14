test_that("SI : same output as previous version with small tree", {
  # Load the simple DEXi tree needed for the test
  list_tree <- readRDS(system.file("testdata", "testtrees.rds",
                                   package = "dexisensitivity"
  ))
  tree <- list_tree[[1]]

  # Setup a random seed for the test
  set.seed(42)

  # Unit test
  test_output <- si_dexi(tree, is_file = FALSE)

  expected_output <- readRDS(system.file("testdata", "test_si_small.rds",
                                         package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)

  # restore random seed
  set.seed(NULL)
})


test_that("SI : same output as previous version with masc2", {
  # Load the simple DEXi tree needed for the test
  tree <- dexisensitivity::masc2

  # Setup a random seed for the test
  set.seed(42)

  # Unit test
  test_output <- si_dexi(tree, is_file = FALSE)

  expected_output <- readRDS(system.file("testdata", "masc2_si.rds",
                                         package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)

  # restore random seed
  set.seed(NULL)
})


test_that("SI : same output as previous version with dexifruits_v1", {
  # Load the simple DEXi tree needed for the test
  tree <- dexisensitivity::dexifruits_v1

  # Setup a random seed for the test
  set.seed(42)

  # Unit test
  test_output <- si_dexi(tree, is_file = FALSE)

  expected_output <- readRDS(system.file("testdata", "expected_dexifruits_v1_si.rds",
                                         package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)

  # restore random seed
  set.seed(NULL)
})
