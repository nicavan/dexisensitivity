test_that("Same data for synoptic graph with masc2 as previous", {

  # Load masc2 for the test
  tree <- dexisensitivity::masc2

  # Create an option to represent in a synoptic graph
  option <- create_options(tree, 1,seed = 42)

  # Test of internal functions of the function create_synoptique
  # Create dataframe for plotting
  df <- create_data_frame_for_plotting(tree, avoid_repetition = T, depth = NA)

  # Evaluate scenarios and add labels
  df <- evaluate_and_label(df, tree, option)

  # Get coordinates for all boxes
  df2 <- get_box_coordinates(df, depth = NA)

  test_output <- df2

  expected_output <- readRDS(system.file("testdata", "data_synoptic_graph_masc2.rds",
                                         package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)
})


test_that("Same data for synoptic graph with masc2 (case with missing values) as previous", {

  # Load masc2 for the test
  tree <- dexisensitivity::masc2

  # Create an option to represent in a synoptic graph
  option <- create_options(tree, 1,seed = 42)
  # Create a missing value in this option
  option["Efficience energetique",1] <- "*"

  # Test of internal functions of the function create_synoptique
  # Create dataframe for plotting
  df <- create_data_frame_for_plotting(tree, avoid_repetition = T)

  # Evaluate scenarios and add labels
  df <- evaluate_and_label(df, tree, option)

  # Get coordinates for all boxes
  df2 <- get_box_coordinates(df)

  test_output <- df2

  expected_output <- readRDS(system.file("testdata", "data_synoptic_graph_missing_values_masc2.rds",
                                         package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)
})


test_that("Same data for synoptic graph with dexifruits_v1 as previous", {

  # Load dexifruits_v1 for the test
  tree <- dexisensitivity::dexifruits_v1

  # Create an option to represent in a synoptic graph
  option <- create_options(tree, 1, seed = 42)

  # Test of internal functions of the function create_synoptique
  # Create dataframe for plotting
  df <- create_data_frame_for_plotting(tree, avoid_repetition = T)

  # Evaluate scenarios and add labels
  df <- evaluate_and_label(df, tree, option)

  # Get coordinates for all boxes
  df2 <- get_box_coordinates(df)

  test_output <- df2

  expected_output <- readRDS(system.file("testdata", "data_synoptic_graph_dexifruits_v1.rds",
                                         package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)
})


test_that("Same data for synoptic graph with dexifruits_v1 (depth 5) as previous", {

  # Load masc2 for the test
  tree <- dexisensitivity::dexifruits_v1

  # Create an option to represent in a synoptic graph
  option <- create_options(tree, 1, seed = 42)

  # Test of internal functions of the function create_synoptique
  # Create dataframe for plotting
  df <- create_data_frame_for_plotting(tree, depth = 5, avoid_repetition = T)

  # Evaluate scenarios and add labels
  df <- evaluate_and_label(df, tree, option)

  # Get coordinates for all boxes
  df2 <- get_box_coordinates(df, depth = 5)

  test_output <- df2

  expected_output <- readRDS(system.file("testdata", "data_synoptic_graph_dexifruits_v1_depth5.rds",
                                         package = "dexisensitivity"
  ))

  expect_equal(test_output, expected_output)
})
