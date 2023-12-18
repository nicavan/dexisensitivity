#### create_sub_tree tests ####
test_that("same output as previous version with testtree", {
  # Load the simple DEXi tree needed for the test
  load(system.file("testdata", "test_tree.rda", package = "dexiranalysis"))

  ### - Unit test - ###
  # Note :
  #   We used a character object and apply xmlDeserializeHook function
  # to transform it in an external pointer (usual object for this function)
  # since we can't save an external pointer directly.
  tree <- create_tree(XML::xmlDeserializeHook(TestTree))

  test_output <- create_sub_tree(tree[[1]], tree[[1]]@Nodes[[2]]@Name)

  expected_output <- readRDS(system.file("testdata", "test_subtree_mini.rds",
    package = "dexiranalysis"
  ))

  expect_equal(test_output, expected_output)
})

test_that("same output as previous version with masc2", {
  masc2 <- dexiranalysis::masc2

  test_output <- create_sub_tree(masc2, masc2@Nodes[[2]]@Name)

  expected_output <- readRDS(system.file("testdata", "subtree_masc2.RDS",
                                         package = "dexiranalysis"
  ))

  expect_equal(test_output, expected_output)
})

test_that("create subtree with main root", {
  masc2 <- dexiranalysis::masc2

  test_output <- create_sub_tree(masc2, masc2@RootName)

  expected_output <- masc2

  expect_equal(test_output, expected_output)
})

