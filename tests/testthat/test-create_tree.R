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

  attributes <- get_dexi_attributes(main_tree, root_name)

  test_output <- get_paths(attributes, masc2_xml, root_name)

  expected_output <- readRDS(system.file("testdata", "expected_get_paths_masc2",
                                        package = "dexiranalysis"
  ))

  # We expect error for everything other than character vector of length 1
  expect_equal(test_output, expected_output)
})
