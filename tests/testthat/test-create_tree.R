test_that("same output as JEB's scripts", {
  # Load the simple DEXi tree needed for the test
  load(system.file("testdata", "TestTree.rda", package = "dexiranalysis"))

  ### - Unit test - ###
  # Note :
  #   We used a character object and apply xmlDeserializeHook function
  # to transform it in an external pointer (usual object for this function)
  # since we can't save an external pointer directly.
  test_output <- create_tree(XML::xmlDeserializeHook(TestTree))
  list_output <- readRDS(system.file("testdata", "listDEXi.rds",
    package = "dexiranalysis"
  ))

  expected_output <- list_output[[1]]

  ### - End - ###

  expect_equal(test_output[[1]], expected_output)
})


test_that("same output as JEB's scripts 2", {
  # Load the complex DEXi tree needed for the test
  tree <- readRDS(system.file("testdata", "TestMTDEXiPM.rds",
    package = "dexiranalysis"
  ))

  ### - Unit test - ###
  # Note :
  #   We used a character object and apply xmlDeserializeHook function
  # to transform it in an external pointer (usual object for this function)
  # since we can't save an external pointer directly.
  test_output <- create_tree(XML::xmlDeserializeHook(tree))
  list_output <- readRDS(system.file("testdata", "TestDEXiPM.rds",
    package = "dexiranalysis"
  ))
  expected_output <- list_output[[1]]

  ### - End - ###

  expect_equal(test_output[[1]], expected_output)
})
