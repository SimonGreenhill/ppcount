test_that("Test get_rootheight - ape::phylo", {
    age <- get_rootheight(ape::read.nexus("test_rescale_tree.trees"))
    expect_equal(age, 998.4147457347)
})

test_that("Test get_rootheight - treeio::treedata", {
    age <- get_rootheight(treeio::read.beast("test_rescale_tree.trees"))
    expect_equal(age, 998.4147457347)
})

test_that("Test get_rootheight - non-ultrametric", {
    tree <- ape::read.tree(text="((t4:0.1707912828,(t2:0.9369924723,t1:0.4300128343):0.3816403393):0.1595276098,t3:0.5288363316);")
    age <- get_rootheight(tree)
    expect_equal(age, 1.47816042)
})
