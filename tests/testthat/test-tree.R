# Tests scripts for Tree class #
# # # # # # # # # # # # # # # #


#### Tree class test ####
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



#### print method test ####
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


test_that("masc2 print correctly", {
  test_print <- capture.output(print(dexiranalysis::masc2))

  expected_print <- capture.output({
    cat("Root name: Contribution au developpement durable")
    cat("\nNumber of attributes: 65")
    cat("\nNumber of aggregated attributes: 26")
    cat("\nNumber of true leaves (no multiple, no aggregated): 39")
    cat("\nMaximum depth: 6")
    cat("\nList of repeated aggregated nodes: Non")
    cat("\nNo multiple leaves")
    cat("\nNo Leaf-Aggregated Leaf")
  })

  expect_equal(test_print, expected_print)

})

#### show method test ####
test_that("Empty Tree show correct message", {
  expect_equal(
    capture.output(new(Class = "Tree")),
    "*** Tree without attributes ***"
  )
})


test_that("masc2 show correctly", {
  test_print <- capture.output(dexiranalysis::masc2)

  expected_print <- capture.output({
  cat("<  1 > Z : Contribution au developpement durable\n")
  cat("<  2 > - Y : Dimension economique\n")
  cat("<  3 > - - Y : Resultats economiques\n")
  cat("<  4 > - - - X : Rentabilite\n")
  cat("<  5 > - - - Y : Autonomie economique\n")
  cat("<  6 > - - - - X : Independance economique\n")
  cat("<  7 > - - - - X : Efficience economique\n")
  cat("<  8 > - - - X : Surcout en materiel\n")
  cat("<  9 > - - Y : Capacite productive a long terme\n")
  cat("< 10 > - - - Y : Maitrise de la fertilite physico-chimique\n")
  cat("< 11 > - - - - X : Maitrise du statut acido-basique du sol\n")
  cat("< 12 > - - - - X : Maitrise de l etat structural du sol\n")
  cat("< 13 > - - - - X : Maitrise de la fertilite phosphopotassique\n")
  cat("< 14 > - - - Y : Maitrise des bioagresseurs\n")
  cat("< 15 > - - - - X : Maitrise des maladies et ravageurs\n")
  cat("< 16 > - - - - X : Maitrise des adventices\n")
  cat("< 17 > - - Y : Contribution au developpement economique\n")
  cat("< 18 > - - - Y : Qualite des produits\n")
  cat("< 19 > - - - - X : Qualite sanitaire\n")
  cat("< 20 > - - - - X : Qualite technologique et esthetique des produits\n")
  cat("< 21 > - - - X : Contribution a l emergence de filieres\n")
  cat("< 22 > - Y : Dimension sociale\n")
  cat("< 23 > - - Y : Satisfaction des attentes de la societe\n")
  cat("< 24 > - - - X : Contribution a l emploi\n")
  cat("< 25 > - - - X : Fourniture de matieres premieres\n")
  cat("< 26 > - - Y : Satisfaction des attentes de l agriculteur\n")
  cat("< 27 > - - - Y : Facilite de mise en oeuvre\n")
  cat("< 28 > - - - - X : Complexite des itineraires techniques\n")
  cat("< 29 > - - - - X : Temps de veille technico-economique\n")
  cat("< 30 > - - - Y : Qualite des conditions de travail\n")
  cat("< 31 > - - - - X : Surcharge de travail\n")
  cat("< 32 > - - - - X : Risque pour la sante de l applicateur\n")
  cat("< 33 > - - - - X : Difficulte physique\n")
  cat("< 34 > - Y : Dimension environnementale\n")
  cat("< 35 > - - Y : Contribution a la qualite du milieu\n")
  cat("< 36 > - - - Y : Contribution a la qualite de l eau\n")
  cat("< 37 > - - - - Y : Maitrise des pertes de pesticides Eaux\n")
  cat("< 38 > - - - - - X : Maitrise pertes dans les eaux profondes\n")
  cat("< 39 > - - - - - X : Maitrise pertes dans les eaux superficielles\n")
  cat("< 40 > - - - - X : Maitrise des pertes de NO3\n")
  cat("< 41 > - - - - X : Maitrise des pertes de P\n")
  cat("< 42 > - - - Y : Contribution a la qualite air\n")
  cat("< 43 > - - - - X : Maitrise des emissions de NH3\n")
  cat("< 44 > - - - - X : Maitrise des emissions de N2O\n")
  cat("< 45 > - - - - X : Maitrise des emissions de pesticides Air\n")
  cat("< 46 > - - - Y : Preservation de la qualite du sol\n")
  cat("< 47 > - - - - X : Maitrise de l accumulation d elements toxiques\n")
  cat("< 48 > - - - - X : Maitrise du statut organique\n")
  cat("< 49 > - - - - X : Maitrise de l erosion\n")
  cat("< 50 > - - Y : Pression sur les ressources abiotiques\n")
  cat("< 51 > - - - Y : Pression Eau\n")
  cat("< 52 > - - - - X : Conso. en eau d irrigation en periode critique\n")
  cat("< 53 > - - - - X : Dependance vis a vis de la ressource en eau\n")
  cat("< 54 > - - - Y : Pression Energie\n")
  cat("< 55 > - - - - X : Consommation en energie\n")
  cat("< 56 > - - - - X : Efficience energetique\n")
  cat("< 57 > - - - X : Pression Phosphore\n")
  cat("< 58 > - - Y : Conservation de la biodiversite\n")
  cat("< 59 > - - - Y : Conservation de la macrofaune\n")
  cat("< 60 > - - - - X : Conservation des insectes volants\n")
  cat("< 61 > - - - - X : Conservation de la macrofaune du sol\n")
  cat("< 62 > - - - Y : Conservation de la flore\n")
  cat("< 63 > - - - - X : Abondance floristique\n")
  cat("< 64 > - - - - X : Diversite floristique\n")
  cat("< 65 > - - - X : Conservation des micro-organismes du sol\n")
  })

  expect_equal(test_print, expected_print)

})

#### count_digits test ####
test_that("count_digits returns correct number of digits", {
  # Test avec nombre de chiffres différents
  expect_equal(count_digits(0), 1)
  expect_equal(count_digits(1), 1)
  expect_equal(count_digits(1.5), 1)
  expect_equal(count_digits(10), 2)
  expect_equal(count_digits(100), 3)
  expect_equal(count_digits(1000), 4)
})

test_that("count_digits can't handles non-integer input", {
  # Vérifier que la fonction retourne une erreur pour les inputs négatifs ou non
  # numeric
  expect_warning(count_digits(-1))
  expect_error(count_digits("a string"))
  expect_error(count_digits(complex(real = 1, imaginary = 1)))
})


#### describe method test ####
test_that("describe return correct error if no Nodes", {
  empty_tree <- new(Class = "Tree")

  expect_error(describe(empty_tree), "Tree without any node!")
})


test_that("Tree describe correct message with masc2", {
  # Load the complex DEXi tree needed for the test
  test_output <- dexiranalysis::masc2 |>
    describe() |>
    capture.output() # Because we'll compare cat() outputs

  expected_output <- readRDS(system.file("testdata", "describe_masc2.rds",
                                         package = "dexiranalysis"
  ))

  expect_equal(test_output, expected_output)
})

