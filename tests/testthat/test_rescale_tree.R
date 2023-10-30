library(ape)
library(treeio)

test_that("Test rescale_tree (bad input)", {
    expect_error(rescale_tree("(1,2)", 1), "Unhandled tree type", fixed=TRUE)
})


test_that("Test rescale_tree - ape::phylo", {
    tree <- ape::read.tree(text = "((t4:1,t5:2):3,((t3:4,t1:5):6,t2:7):8);")
    expect_equal(tree$edge.length, rescale_tree(tree, 1)$edge.length)
})


test_that("Test rescale_tree - ape::phylo (scaled=1000)", {
    tree <- ape::read.tree(text = "((t4:1,t5:2):3,((t3:4,t1:5):6,t2:7):8);")
    expect_equal(tree$edge.length * 1000, rescale_tree(tree, 1000)$edge.length)
})


test_that("Test rescale_tree - treeio::treedata - identical", {
    tree <- treeio::read.beast("test_rescale_tree.trees")
    expect_equal(tree@phylo$edge.length, rescale_tree(tree, 1)@phylo$edge.length)

    rescaled_tree <- rescale_tree(tree, 1)

    # Unchanged
    for (col in c("node", "posterior")) {
        expect_equal(tree@data[[col]], rescaled_tree@data[[col]], info=col)
    }
    # Changed
    for (col in c("height", "height_median", "length", "length_median", 'rate', 'rate_median')) {
        expect_equal(as.numeric(tree@data[[col]]), rescaled_tree@data[[col]], info=col)
    }

    # Multiples
    for (col in c("height_range",  "length_range", "rate_range", "height_0.95_HPD", "length_0.95_HPD", 'rate_0.95_HPD')) {
        expect_equal(as.numeric(unlist(tree@data[[col]])), unlist(rescaled_tree@data[[col]]), info=col)
    }
})


test_that("Test rescale_tree - treeio::treedata - (scaler=1000)", {
    tree <- treeio::read.beast("test_rescale_tree.trees")
    rescaled_tree <- rescale_tree(tree, 1000)

    expect_equal(tree@phylo$edge.length, rescaled_tree@phylo$edge.length / 1000, info=col)

    # Unchanged
    for (col in c("node", "posterior")) {
        expect_equal(tree@data[[col]], rescaled_tree@data[[col]], info=col)
    }
    # Changed
    for (col in c("height", "height_median", "length", "length_median")) {
        expect_equal(as.numeric(tree@data[[col]]), rescaled_tree@data[[col]] / 1000, info=col)
    }
    expect_equal(as.numeric(tree@data[['rate']]), rescaled_tree@data[['rate']] * 1000, info=col)
    expect_equal(as.numeric(tree@data[['rate_median']]), rescaled_tree@data[['rate_median']] * 1000, info=col)

    # Multiples
    for (col in c("height_range",  "length_range", "height_0.95_HPD", "length_0.95_HPD")) {
        expect_equal(as.numeric(unlist(tree@data[[col]])), unlist(rescaled_tree@data[[col]]) / 1000)
    }
    expect_equal(as.numeric(unlist(tree@data[['rate_0.95_HPD']])), unlist(rescaled_tree@data[['rate_0.95_HPD']]) * 1000)
    expect_equal(as.numeric(unlist(tree@data[['rate_range']])), unlist(rescaled_tree@data[['rate_range']]) * 1000)
})

