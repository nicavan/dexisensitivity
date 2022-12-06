test_that("same output as JEB's scripts", {
    load(system.file("testdata", "TestTree.rda", package="dexiranalysis"))
    out1 <- createTree(XML::xmlDeserializeHook(TestTree))
    out2 <- readRDS(system.file("testdata", "listDEXi.rds", package="dexiranalysis"))

    expect_equal(out1, out2)
})


test_that("same output as JEB's scripts 2", {

    PMtest <- readRDS(system.file("testdata", "TestMTDEXiPM.rds", package="dexiranalysis"))
    out1 <- createTree(XML::xmlDeserializeHook(PMtest))
    out2 <- readRDS(system.file("testdata", "TestDEXiPM.rds", package="dexiranalysis"))

    expect_equal(out1, out2)
})
