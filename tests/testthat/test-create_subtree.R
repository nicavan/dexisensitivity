test_that("same output as JEB's scripts", {
    # Load the simple DEXi tree needed for the test
    load(system.file("testdata", "TestTree.rda", package = "dexiranalysis"))

    ### - Unit test - ###
    # Note :
    #   We used a character object and apply xmlDeserializeHook function
    # to transform it in an external pointer (usual object for this function)
    # since we can't save an external pointer directly.
    Trees <- createTree(XML::xmlDeserializeHook(TestTree))
    out1 <- createSubTree(Trees[[1]], Trees[[1]]@Nodes[[2]]@name)

    out2 <- readRDS(system.file("testdata", "Testsubtreemini.rds",
                                package = "dexiranalysis"))

    expect_equal(out1, out2)
})


test_that("same output as JEB's scripts 2", {
    # Load the complex DEXi tree needed for the test
    PMtest <- readRDS(system.file("testdata", "TestMTDEXiPM.rds",
                                  package = "dexiranalysis"))

    ### - Unit test - ###
    # Note :
    #   We used a character object and apply xmlDeserializeHook function
    # to transform it in an external pointer (usual object for this function)
    # since we can't save an external pointer directly.
    PMTree <- createTree(XML::xmlDeserializeHook(PMtest))
    out1 <- createSubTree(PMTree[[1]], PMTree[[1]]@Nodes[[2]]@name)

    out2 <- readRDS(system.file("testdata", "TestsubtreeDEXiPM.rds",
                                package = "dexiranalysis"))

    expect_equal(out1, out2)
})
