test_that("same output as JEB's scripts", {
    load(system.file("testdata", "TestTree.rda", package="dexiranalysis"))
    out1 <- createTree(XML::xmlDeserializeHook(TestTree))
    out2 <- readRDS(system.file("testdata", "listDEXi.rds", package="dexiranalysis"))

    expect_equal(out1, out2)
})



