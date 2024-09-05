#### create_tree tests ####
test_that("Same output as previous versions with XML", {
  # get .dxi for masc2 path
  dxi_masc2_path <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                                package = "dexiranalysis")

  # expected output : a previously generated masc2 Tree
  expected_output <- readRDS(system.file("testdata", "expected_masc2_tree.rds",
                                         package = "dexiranalysis"
  ))

  # Note :
  #   We used a character object and apply xmlDeserializeHook function
  # to transform it in an external pointer (usual object for this function)
  # since we can't save an external pointer directly.
  test_output_xml <- create_tree(XML::xmlDeserializeHook(dxi_masc2_path))[[1]]
  test_output_path <- create_tree(dxi_masc2_path)[[1]]


  expect_equal(test_output_xml, expected_output)
  expect_equal(test_output_path, expected_output)

})

test_that("Same output as previous versions with .dxi path", {
  # get .dxi for masc2 path
  dxi_masc2_path <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                                package = "dexiranalysis")

  # expected output : a previously generated masc2 Tree
  expected_output <- readRDS(system.file("testdata", "expected_masc2_tree.rds",
                                         package = "dexiranalysis"
  ))

  # Note :
  #   We used a character object and apply xmlDeserializeHook function
  # to transform it in an external pointer (usual object for this function)
  # since we can't save an external pointer directly.
  pre_output <- create_tree(dxi_masc2_path)
  test_output <- pre_output[[1]]


  expect_equal(test_output, expected_output)
})

test_that("Same output as previous versions with .dxi path for a complex tree", {
  # get .dxi for dexifruits_v1 path
  dxi_dexifruits_v1_path <- system.file("extdata", "DEXiFruits_V1.dxi",
                                package = "dexiranalysis")

  # expected output : a previously generated dexifruits_v1 Tree
  expected_output <- readRDS(system.file("testdata", "expected_dexifruits_v1_tree.rds",
                                         package = "dexiranalysis"
  ))

  # use of the create_tree function, with choosing of one root, and correction of
  # issues with special characters.
  pre_output <- create_tree(dxi_dexifruits_v1_path,
                            which_root = 1,
                            correct = T)
  test_output <- pre_output[[1]]


  expect_equal(test_output, expected_output)
})


#### create_tree helpers tests ####
test_that("get_dexi_attributes give correct output", {
  # get masc2 xml
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  # get all attributes of "Pression Energie"
  test_output <- dexiranalysis:::get_dexi_attributes(masc2_xml, "Pression Energie")

  expected_output <- c("Pression Energie", "Consommation en energie", "Efficience energetique")


  expect_equal(test_output, expected_output)
})


test_that("get_dexi_attributes give warning but still return root name if no descendant", {
  # get masc2 xml
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  # use with false attribute
  test_output <- dexiranalysis:::get_dexi_attributes(masc2_xml, "bla") |>
    suppressWarnings()

  expected_output <- c("bla")


  expect_equal(test_output, expected_output)
  expect_warning(dexiranalysis:::get_dexi_attributes(masc2_xml, "bla"))
})


test_that("get_dexi_attributes correctly give error message", {
  # get masc2 xml
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  # We expect error for everything other than character vector of length 1
  expect_error(dexiranalysis:::get_dexi_attributes(masc2_xml, 1))
  expect_error(dexiranalysis:::get_dexi_attributes(masc2_xml, c("bla", "Bla")))
})


test_that("get_paths give correct output", {
  # get masc2 xml
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"

  attributes <- get_dexi_attributes(masc2_xml, root_name)


  test_output <- get_paths(attributes, masc2_xml, root_name)

  expected_output <- readRDS(system.file("testdata", "expected_get_paths_masc2.RDS",
                                         package = "dexiranalysis"
  ))


  # We expect error for everything other than character vector of length 1
  expect_equal(test_output, expected_output)
})


test_that("process_aggregated_leaf give correct output", {
  # get a node from masc2
  masc2 <- dexiranalysis::masc2

  # get leaves and aggregated
  leaves <- sapply(
    masc2@Nodes,
    function(x) {
      if (x@IsLeaf) x@Name
    }
  ) |>
    unlist()

  aggregated <- sapply(
    masc2@Nodes,
    function(x) {
      if (!x@IsLeaf) x@Name
    }
  ) |>
    unlist()


  # Unit test for all nodes
  for(i in 1:masc2@NumberOfAttributes) {
    test_output <- process_aggregated_leaf(
      masc2@Nodes[[i]],
      leaves,
      aggregated
    )

    expected_output <- list(leaf_aggregated = character(0),
                            is_leaf_aggregated = FALSE,
                            tree_nodes = masc2@Nodes[[i]])


    # We expect error for everything other than character vector of length 1
    expect_equal(test_output, expected_output)
  }
})


test_that("process_duplicated_leaf give correct output", {
  # get a node from masc2
  masc2 <- dexiranalysis::masc2

  # get leaves and aggregated
  leaves <- sapply(
    masc2@Nodes,
    function(x) {
      if (x@IsLeaf) x@Name
    }
  ) |>
    unlist()


  # Unit test for all nodes
  for(i in 1:masc2@NumberOfAttributes) {
    test_output <- process_duplicated_leaf(
      masc2@Nodes[[3]],
      leaves
    )

    expected_output <- list(multiple = data.frame(Occ = NA),
                            is_multiple = FALSE,
                            tree_nodes = masc2@Nodes[[3]])


    # We expect error for everything other than character vector of length 1
    expect_equal(test_output, expected_output)
  }
})


#### create_node tests ####
test_that("Same output as previous versions", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"
  attributes <- get_dexi_attributes(masc2_xml, root_name)
  list_path <- get_paths(attributes, masc2_xml, root_name)

  # masc2 contains all expected nodes outputs
  masc2 <- dexiranalysis::masc2

  for(i in 1:length(list_path)) {
    test_output <- create_node(list_path[[i]], masc2_xml)

    # When creating a masc Tree, we also attribute Id.
    # So we need to setup it with initial value : numerci(0)
    expected_output <- masc2@Nodes[[i]]
    expected_output@Id <- numeric(0)
    expect_equal(test_output, expected_output)
  }

})


#### create_node helpers tests ####


test_that("determine_leaf_status give correct output", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"
  attributes <- get_dexi_attributes(masc2_xml, root_name)
  list_path <- get_paths(attributes, masc2_xml, root_name)

  # masc2 contains all expected nodes outputs
  masc2 <- dexiranalysis::masc2

  for(i in 1:length(list_path)) {
    test_output <- determine_leaf_status(list_path[[i]], masc2_xml)

    expected_output <- masc2@Nodes[[i]]@IsLeaf

    expect_equal(test_output, expected_output)
  }
})


test_that("determine_children give correct output", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"
  attributes <- get_dexi_attributes(masc2_xml, root_name)
  list_path <- get_paths(attributes, masc2_xml, root_name)

  # masc2 contains all expected nodes outputs
  masc2 <- dexiranalysis::masc2

  for(i in 1:length(list_path)) {
    is_leaf <- determine_leaf_status(list_path[[i]], masc2_xml)
    test_output <- determine_children(list_path[[i]], masc2_xml, is_leaf)

    expected_output <- masc2@Nodes[[i]]@Children

    expect_equal(test_output, expected_output)
  }
})


test_that("determine_leaf_status give correct output", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"
  attributes <- get_dexi_attributes(masc2_xml, root_name)
  list_path <- get_paths(attributes, masc2_xml, root_name)

  # masc2 contains all expected nodes outputs
  masc2 <- dexiranalysis::masc2

  for(i in 1:length(list_path)) {
    test_output <- determine_mother(list_path[[i]])

    expected_output <- masc2@Nodes[[i]]@Mother

    expect_equal(test_output, expected_output)
  }
})


test_that("determine_sisters give correct output", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"
  attributes <- get_dexi_attributes(masc2_xml, root_name)
  list_path <- get_paths(attributes, masc2_xml, root_name)

  # masc2 contains all expected nodes outputs
  masc2 <- dexiranalysis::masc2

  for(i in 1:length(list_path)) {
    test_output <- determine_sisters(list_path[[i]], masc2_xml)

    expected_output <- masc2@Nodes[[i]]@Sisters

    expect_equal(test_output, expected_output)
  }
})


test_that("determine_scale_node give correct output", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"
  attributes <- get_dexi_attributes(masc2_xml, root_name)
  list_path <- get_paths(attributes, masc2_xml, root_name)

  # masc2 contains all expected nodes outputs
  masc2 <- dexiranalysis::masc2

  for(i in 1:length(list_path)) {
    test_output <- determine_scale_node(list_path[[i]], masc2_xml)

    expected_output <- masc2@Nodes[[i]]@RangeScale

    expect_equal(test_output, expected_output)
  }
})


test_that("determine_scale_label give correct output", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"
  attributes <- get_dexi_attributes(masc2_xml, root_name)
  list_path <- get_paths(attributes, masc2_xml, root_name)

  # masc2 contains all expected nodes outputs
  masc2 <- dexiranalysis::masc2

  for(i in 1:length(list_path)) {
    test_output <- determine_scale_label(list_path[[i]], masc2_xml)

    expected_output <- masc2@Nodes[[i]]@ScaleLabel

    expect_equal(test_output, expected_output)
  }
})


test_that("determine_aggregation give correct output", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"
  attributes <- get_dexi_attributes(masc2_xml, root_name)
  list_path <- get_paths(attributes, masc2_xml, root_name)

  # masc2 contains all expected nodes outputs
  masc2 <- dexiranalysis::masc2

  for(i in 1:length(list_path)) {
    is_leaf <- determine_leaf_status(list_path[[i]], masc2_xml)
    children <- determine_children(list_path[[i]], masc2_xml, is_leaf)
    test_output <- determine_aggregation(list_path[[i]], masc2_xml, is_leaf, children)

    expected_output <- masc2@Nodes[[i]]@Aggregation

    expect_equal(test_output, expected_output)
  }
})


test_that("determine_weight_list give correct output", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  root_name <- "Contribution au developpement durable"
  attributes <- get_dexi_attributes(masc2_xml, root_name)
  list_path <- get_paths(attributes, masc2_xml, root_name)

  # masc2 contains all expected nodes outputs
  masc2 <- dexiranalysis::masc2

  for(i in 1:length(list_path)) {
    is_leaf <- determine_leaf_status(list_path[[i]], masc2_xml)
    children <- determine_children(list_path[[i]], masc2_xml, is_leaf)
    aggregation <- determine_aggregation(list_path[[i]], masc2_xml, is_leaf, children)
    test_output <- determine_weight_list(list_path[[i]], masc2_xml, is_leaf, aggregation, children)

    expected_output <- masc2@Nodes[[i]]@Probability

    expect_equal(test_output, expected_output)
  }
})


test_that("get_chaine give correct output", {
  # get masc2 in XML
  masc2_xml <- system.file("extdata", "arborescence_MASC_2_0.dxi",
                           package = "dexiranalysis") |>
    XML::xmlDeserializeHook()

  node_path <- "test path"
  node_chain <- c("test path", "test path bis")

  test_output_path <- get_chaine(node_path)
  test_output_chain <- get_chaine(node_chain)

  expected_output_path <- "//ATTRIBUTE[NAME='test path']"
  expected_output_chain <- "//ATTRIBUTE[NAME='test path']/ATTRIBUTE[NAME='test path bis']"

  expect_equal(test_output_path, expected_output_path)
  expect_equal(test_output_chain, expected_output_chain)
})


test_that("get_id give correct output", {
  # get masc2 in XML
  masc2 <- dexiranalysis::masc2

  # For better comprehension, we test the name of the attribute
  for(i in 1:length(masc2@Nodes)) {
    test_id <- get_id(masc2@Nodes, masc2@Attributes[i])
    test_output <- masc2@Nodes[[test_id]]@Name

    expected_output <- masc2@Attributes[[i]]

    expect_equal(test_output, expected_output)
  }
})


test_that("evaluate_order give correct output", {
  # get masc2 in XML
  masc2 <- dexiranalysis::masc2

  test_output <- evaluate_order(masc2)

  expected_output <- numeric(0) # Because no aggregtedleaf

  expect_equal(test_output, expected_output)
})


test_that("get_leaves give correct output", {
  # get masc2 in XML
  masc2 <- dexiranalysis::masc2

  test_output <- get_leaves(masc2, 3)

  expected_output <- c(4, 6, 7, 8)

  expect_equal(test_output, expected_output)
})
