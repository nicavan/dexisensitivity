# Tests scripts for Node class #
# # # # # # # # # # # # # # # #

#### General tests ####
test_that("Node is S4", {
  expect_equal(sloop::otype(new(Class = "Node")), "S4")
})


test_that("Empty Node return structured Node with 0 length S4 attributes", {
  empty_Node <- new(Class = "Node")
  expect_equal(empty_Node@Id, new(Class = "numeric"))
  expect_equal(empty_Node@Name, new(Class = "character"))
  expect_equal(empty_Node@IsLeaf, new(Class = "logical"))
  expect_equal(empty_Node@IsLeafAndAggregated, new(Class = "logical"))
  expect_equal(empty_Node@Children, new(Class = "character"))
  expect_equal(empty_Node@Sisters, new(Class = "character"))
  expect_equal(empty_Node@Mother, new(Class = "character"))
  expect_equal(empty_Node@Aggregation, new(Class = "matrix"))
  expect_equal(empty_Node@Probability, new(Class = "numeric"))
  expect_equal(empty_Node@Depth, new(Class = "numeric"))
  expect_equal(empty_Node@Twin, new(Class = "numeric"))
  expect_equal(empty_Node@ConditionalProbabilityList, new(Class = "list"))
  expect_equal(empty_Node@RangeScale, new(Class = "numeric"))
  expect_equal(empty_Node@ScaleLabel, new(Class = "character"))
  expect_equal(empty_Node@NodePath, new(Class = "character"))
})


#### print method test ####
test_that("Empty Node print correctly", {
  empty_node <- new(Class = "Node")

  empty_print <- capture.output({
    cat("Node name: ")
    cat("\nID: ")
    cat("\nNode depth: ")
    cat("\nFrom root to node: \n  ")
    cat("\nIs it a leaf: ")
    cat("\nIs is a leaf-aggregated: FALSE")
    cat("\nMother: ")
    cat("\nSisters: None")
    cat("\nChildren: None")
    cat("\nEstimated weights: ")
  })

  expect_equal(capture.output(print(empty_node)), empty_print)
})

test_that("masc2 Node print correctly", {
  masc2_node <- dexiranalysis::masc2@Nodes[[42]]

  empty_print <- capture.output({
    cat("Node name: Contribution a la qualite air")
    cat("\nID: 42")
    cat("\nNode depth: 4")
    cat("\nFrom root to node: \n  Contribution au developpement durable -> Dimension environnementale -> Contribution a la qualite du milieu -> Contribution a la qualite air")
    cat("\nIs it a leaf: FALSE")
    cat("\nIs is a leaf-aggregated: FALSE")
    cat("\nMother: Contribution a la qualite du milieu")
    cat("\nSisters: Contribution a la qualite de l eau Preservation de la qualite du sol")
    cat("\nChildren: Maitrise des emissions de NH3 Maitrise des emissions de N2O Maitrise des emissions de pesticides Air")
    cat("\nEstimated weights: 30 40 30")
  })

  expect_equal(capture.output(print(masc2_node)), empty_print)
})


#### compute_leaf_weights ####

test_that("compute_leaf_weights : same output as JEB's scripts", {
  masc2 <- dexiranalysis::masc2

  # Unit test
  expect_equal(
    round(compute_leaf_weights(masc2@Nodes[[1]]), digits = 7),
    c(
      "xDimension economique" = 0.3358491,
      "xDimension sociale" = 0.3320755,
      "xDimension environnementale" = 0.3320755
    )
  )
})


#### create_aggregation_matrix ####

test_that("same output as JEB's scripts for masc2", {

  original_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  masc2 <- dexiranalysis::masc2
  myWeights <- c(0.2, 0.2, 0.6)

  # Setup a random seed for the test
  set.seed(42)

  ### - Unit test - ###
  test_output <- create_aggregation_matrix(masc2@Nodes[[1]], myWeights, 5)

  expected_output <- readRDS(system.file("testdata",
                                         "aggregation_matrix_masc2.rds",
                                         package = "dexiranalysis"
  ))

  expect_equal(test_output, expected_output)
  ### - End - ###

  # restore random seed
  if (!is.null(original_seed)) {
    assign(".Random.seed", original_seed, envir = .GlobalEnv)
  } else {
    rm(.Random.seed, envir = .GlobalEnv)
  }
})
