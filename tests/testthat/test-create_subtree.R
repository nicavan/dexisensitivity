test_that("same output as JEB's scripts", {
  # Load the simple DEXi tree needed for the test
  load(system.file("testdata", "TestTree.rda", package = "dexiranalysis"))

  ### - Unit test - ###
  # Note :
  #   We used a character object and apply xmlDeserializeHook function
  # to transform it in an external pointer (usual object for this function)
  # since we can't save an external pointer directly.
  tree <- create_tree(XML::xmlDeserializeHook(TestTree))

  test_output <- create_sub_tree(tree[[1]], tree[[1]]@Nodes[[2]]@Name)

  expected_output <- readRDS(system.file("testdata", "Testsubtreemini.rds",
    package = "dexiranalysis"
  ))

  expect_equal(test_output, expected_output)
})


test_that("same output as JEB's scripts 2", {
  # Load the complex DEXi tree needed for the test
  xmltree <- readRDS(system.file("testdata", "TestMTDEXiPM.rds",
    package = "dexiranalysis"
  ))

  ### - Unit test - ###
  # Note :
  #   We used a character object and apply xmlDeserializeHook function
  # to transform it in an external pointer (usual object for this function)
  # since we can't save an external pointer directly.
  tree <- create_tree(XML::xmlDeserializeHook(xmltree))
  test_output <- create_sub_tree(tree[[1]], tree[[1]]@Nodes[[2]]@Name)

  expected_output <- readRDS(system.file("testdata", "TestsubtreeDEXiPM.rds",
    package = "dexiranalysis"
  ))

  expect_equal(test_output, expected_output)
})
