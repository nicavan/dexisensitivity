# Tests scripts for Tree class #
################################

test_that("Tree is S4", {
    expect_equal(sloop::otype(new(Class = "Tree")),"S4")
})

# Pour voir ce que fait un test qui fail ou avec error
# test_that("Tree is S4", {
#     expect_equal(otype(),"S4")
# })

