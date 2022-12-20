test_that("same output as JEB's scripts", {
    load(system.file("testdata", "TestTree.rda", package="dexiranalysis"))

    Trees <- createTree(XML::xmlDeserializeHook(TestTree))
    out1 <- createSubTree(Trees[[1]], Trees[[1]]@Nodes[[2]]@name)

    out2 <- readRDS(system.file("testdata", "Testsubtreemini.rds",
                                package="dexiranalysis"))

    expect_equal(out1 , out2)
})


test_that("same output as JEB's scripts 2", {

    PMtest <- readRDS(system.file("testdata", "TestMTDEXiPM.rds",
                                  package="dexiranalysis"))
    PMTree <- createTree(XML::xmlDeserializeHook(PMtest))
    out1 <- createSubTree(PMTree[[1]], PMTree[[1]]@Nodes[[2]]@name)

    out2 <- readRDS(system.file("testdata", "TestsubtreeDEXiPM.rds",
                                package="dexiranalysis"))

    expect_equal(out1, out2)
})
