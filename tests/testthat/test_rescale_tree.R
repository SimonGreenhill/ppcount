library(ape)
library(treeio)

context("Test rescale_tree (bad input)")
test_that("Test rescale_tree (bad input)", {
    expect_error(rescale_tree("(1,2)", 1), "Unhandled tree type", fixed=TRUE)
})


context("Test rescale_tree - ape::phylo (scaled=1)")
test_that("Test rescale_tree - ape::phylo", {
    tree <- ape::read.tree(text = "((t4:1,t5:2):3,((t3:4,t1:5):6,t2:7):8);")
    expect_equal(tree$edge.length, rescale_tree(tree, 1)$edge.length)
})


test_that("Test rescale_tree - ape::phylo (scaled=1000)", {
    tree <- ape::read.tree(text = "((t4:1,t5:2):3,((t3:4,t1:5):6,t2:7):8);")
    expect_equal(tree$edge.length * 1000, rescale_tree(tree, 1000)$edge.length)
})


context("Test rescale_tree - treeio::treedata (scaled=1)")
test_that("Test rescale_tree - treeio::treedata - identical", {
    tree <- treeio::read.beast("test_rescale_tree.trees")
    expect_equal(tree@phylo$edge.length, rescale_tree(tree, 1)@phylo$edge.length)

    rescaled_tree <- rescale_tree(tree, 1)

    # Unchanged
    for (col in c("node", "posterior")) {
        expect_equal(tree@data[[col]], rescaled_tree@data[[col]])
    }
    # Changed
    for (col in c("height", "height_median", "length", "length_median", "rate", "rate_median")) {
        expect_equal(tree@data[[col]], rescaled_tree@data[[col]])
    }
    # Multiples
    for (col in c("height_range",  "length_range", "rate_range", "height_0.95_HPD", "length_0.95_HPD", "rate_0.95_HPD")) {
        expect_equal(unlist(tree@data[['height_range']]), unlist(rescaled_tree@data[['height_range']]))
    }
})



context("Test rescale_tree - treeio::treedata (scaled=1000)")
test_that("Test rescale_tree - treeio::treedata - identical", {
    tree <- treeio::read.beast("test_rescale_tree.trees")
    rescaled_tree <- rescale_tree(tree, 1000)

    expect_equal(tree@phylo$edge.length, rescaled_tree@phylo$edge.length / 1000)

    # Unchanged
    for (col in c("node", "posterior")) {
        expect_equal(tree@data[[col]], rescaled_tree@data[[col]])
    }
    # Changed
    for (col in c("height", "height_median", "length", "length_median", "rate", "rate_median")) {
        expect_equal(tree@data[[col]], rescaled_tree@data[[col]] / 1000)
    }
    # Multiples
    for (col in c("height_range",  "length_range", "rate_range", "height_0.95_HPD", "length_0.95_HPD", "rate_0.95_HPD")) {
        expect_equal(unlist(tree@data[['height_range']]), unlist(rescaled_tree@data[['height_range']]) / 1000)
    }
})

