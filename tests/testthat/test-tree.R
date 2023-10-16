# Tests scripts for Tree class #
################################


#### - Tree class test - #### ####
test_that("Tree is S4", {
  expect_equal(sloop::otype(new(Class = "Tree")), "S4")
})



test_that("Empty tree return structured tree with 0 length S4 attributes", {
  empty_tree <- new(Class = "Tree")
  expect_equal(empty_tree@NumberOfAttributes, new(Class = "numeric"))
  expect_equal(empty_tree@NumberOfLeaves, new(Class = "numeric"))
  expect_equal(empty_tree@Depth, new(Class = "numeric"))
  expect_equal(empty_tree@Attributes, new(Class = "character"))
  expect_equal(empty_tree@Leaves, new(Class = "character"))
  expect_equal(empty_tree@Aggregated, new(Class = "character"))
  expect_equal(empty_tree@IsMultiple, new(Class = "logical"))
  expect_equal(empty_tree@Multiple, new(Class = "data.frame"))
  expect_equal(empty_tree@IsLeafAggregated, new(Class = "logical"))
  expect_equal(empty_tree@LeafAggregated, new(Class = "character"))
  expect_equal(empty_tree@Paths, new(Class = "list"))
  expect_equal(empty_tree@Nodes, new(Class = "list"))
  expect_equal(empty_tree@EvaluationOrder, new(Class = "numeric"))
  expect_equal(empty_tree@RootName, new(Class = "character"))
})



#### - print method test - #### ####
test_that("Empty Tree print correctly", {
  empty_tree <- new(Class = "Tree")

  empty_print <- capture.output({
    cat("Root name: ")
    cat("\nNumber of attributes: 0")
    cat("\nNumber of aggregated attributes: 0")
    cat("\nNumber of true leaves (no multiple, no aggregated): ")
    cat("\nMaximum depth: ")
    cat("\nList of repeated aggregated nodes: Non")
    cat("\nNo multiple leaves")
    cat("\nNo Leaf-Aggregated Leaf")
  })

  expect_equal(capture.output(print(empty_tree)), empty_print)
})

#### - show method test - #### ####
test_that("Empty Tree show correct message", {
  expect_equal(
    capture.output(new(Class = "Tree")),
    "*** Tree without attributes ***"
  )
})

#### - calculate_digit test - #### ####
test_that("calculate_digit returns correct number of digits", {
  # Test avec nombre de chiffres différents
  expect_equal(calculate_digit(0), 1)
  expect_equal(calculate_digit(1), 1)
  expect_equal(calculate_digit(1.5), 1)
  expect_equal(calculate_digit(10), 2)
  expect_equal(calculate_digit(100), 3)
  expect_equal(calculate_digit(1000), 4)
})

test_that("calculate_digit can't handles non-integer input", {
  # Vérifier que la fonction retourne une erreur pour les inputs négatifs ou non
  # numeric
  expect_warning(calculate_digit(-1))
  expect_error(calculate_digit("string"))
})


#### - describe method test - #### ####
test_that("describe return correct error if no Nodes", {
  empty_tree <- new(Class = "Tree")

  expect_error(describe(empty_tree), "Tree without any node!")
})

test_that("Empty Tree describe correct message", {
  # Load the complex DEXi tree needed for the test
  tree <- readRDS(system.file("testdata", "TestMTDEXiPM.rds",
    package = "dexiranalysis"
  ))

  # Unit test
  test_output <- create_tree(XML::xmlDeserializeHook(tree))

  expected_output <- readRDS(system.file("testdata", "describeDEXiPM.rds",
    package = "dexiranalysis"
  ))

  expect_equal(capture.output(describe(test_output[[1]])), expected_output)
})

#### - - #### ####
# Pour voir ce que fait un test qui fail ou avec error
# test_that("Tree is S4", {
#     expect_equal(otype(),"S4")
# })
