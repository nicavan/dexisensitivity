test_that("Depth order return correct order for masc2", {
  # Load the complex DEXi tree needed for the test
  masc2 <- dexiranalysis::masc2

  sousArbre <- create_sub_tree(masc2, "Dimension sociale")

  # Test depth order on a subTree
  test_output <- depth_order(sousArbre)

  expected_output <- c(
    "Facilite de mise en oeuvre",
    "Qualite des conditions de travail",
    "Satisfaction des attentes de la societe",
    "Satisfaction des attentes de l agriculteur",
    "Dimension sociale"
  )

  expect_equal(test_output, expected_output)
})
