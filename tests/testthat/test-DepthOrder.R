test_that("Depth order return correct order for DEXiPM", {
    # Load the complex DEXi tree needed for the test
    list_tree <- readRDS(system.file("testdata", "TestDEXiPM.rds",
                                     package = "dexiranalysis"))
    tree <- list_tree[[1]]

    sousArbre <- createSubTree(tree, "Flora")

    # Test depth order on a subTree
    test_output <- depth_order(sousArbre)

    expected_output <- c("Crop type and weeds diversity",
                         "Chemical pressure on flora",
                         "Crop type and weeds abundance",
                         "Margin flora quality",
                         "Weed diversity",
                         "Weed abundance",
                         "Natural semi natural flora",
                         "Weeds",
                         "Flora")

    expect_equal(test_output, expected_output)

})
