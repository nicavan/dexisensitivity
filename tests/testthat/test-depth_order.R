test_that("Depth order return correct order for masc2", {
  # Load the complex DEXi tree needed for the test
  masc2 <- dexisensitivity::masc2

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

test_that("Depth order return correct order for dexifruits_v1", {
  dexifruits_v1 <- dexisensitivity::dexifruits_v1

  # Test depth_order on dexifruits_v1 (a tree with aggregated leaves)
  test_output <- depth_order(dexifruits_v1)

  expected_output <- readRDS(system.file("testdata",
                                         "expected_depth_order_dexifruits_v1.rds",
                                         package = "dexisensitivity"
                            ))

  expect_equal(test_output, expected_output)
})
