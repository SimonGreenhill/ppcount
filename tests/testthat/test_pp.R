library(ape)

sTREE <- "((t4:1,t5:2):3,((t3:4,t1:5):6,t2:7):8);"

sCLADES <- "
POSTERIOR PROBABILITIES FOR ...
taxset t1t3 = t1 t3;
taxset t4t5 = t4 t5;
blah blah
taxset t1t2t3 = t1  t2 t3;
taxset t1t2t3t4t5  = t1 t2 t3 t4 t5;
taxset t1 = t1;
taxset t2 = t2;
"
context("Tests - setup")
tree <- ape::read.tree(text = sTREE)

tmp <- tempfile()
setup({
    writeLines(sCLADES, tmp)
})

teardown({
    unlink(tmp)
})


test_that("Test that the tree is loaded correctly", {
    expect_equal(5, length(tree$tip.label))
})

context("Tests - read_clade_file")
test_that("Test read_clade_file", {
    clades <- read_clade_file(tmp)
    expected <- list(
        c('t1', 't3'),
        c('t4', 't5'),
        c('t1', 't2', 't3'),
        c('t1', 't2', 't3', 't4', 't5'),
        c('t1'), c('t2')
    )
    for (e in expected) {
        clade <- paste(e, collapse = "")
        expect_equal(e, clades[[clade]])
    }
    expect_equal(length(clades), length(expected))
})


context("Test get_ages - tips")
test_that("Test get_ages - tips", {
    ages <- get_ages(tree)
    expect_equal(ages['t1', 'mrca.age'], 5)
    expect_equal(ages['t3', 'mrca.age'], 5)
    expect_equal(ages['t2', 'mrca.age'], 11)
    expect_equal(ages['t4', 'mrca.age'], 16)
    expect_equal(ages['t5', 'mrca.age'], 16)
})


context("Test get_ages - nodes - get_age_for_clade")
test_that("Test get_ages - nodes - get_age_for_clade", {
    ages <- get_ages(tree)
    expect_equal(get_age_for_clade(tree, c('t1', 't3'), ages), 5)  # node 9
    expect_equal(get_age_for_clade(tree, c('t1', 't3', 't2'), ages), 11)  # node 8
    expect_equal(get_age_for_clade(tree, c('t4', 't5'), ages), 16)  # node 7
    expect_equal(get_age_for_clade(tree, c('t1', 't2', 't3', 't4', 't5'), ages), 19)  # node 6
})


context("Test get_ages - nodes - get_age_for_clade (ladderized tree)")
test_that("Test get_ages - nodes - get_age_for_clade (ladderized tree)", {
    ages <- get_ages(ape::ladderize(tree))
    expect_equal(get_age_for_clade(tree, c('t1', 't3'), ages), 5)  # node 9
    expect_equal(get_age_for_clade(tree, c('t1', 't3', 't2'), ages), 11)  # node 8
    expect_equal(get_age_for_clade(tree, c('t4', 't5'), ages), 16)  # node 7
    expect_equal(get_age_for_clade(tree, c('t1', 't2', 't3', 't4', 't5'), ages), 19)  # node 6
})


context("Test process_trees")
test_that("Test process_trees", {
    clades <- read_clade_file(tmp)
    trees <- c(tree, tree)
    res <- process_trees(clades, trees, verbose = FALSE)

    expected <- list(
        't1t3' = c(5, 5),
        't1t2t3t4t5' = c(19, 19)
    )
    for (e in names(expected)) {
        expect_equal(res[res$Clade == e,]$Age, expected[[e]])
        expect_equal(res[res$Clade == e,]$Monophyly, c(1, 1))
    }

    # test verbose flag
    expect_message(
        process_trees(clades, trees, verbose = TRUE),
        "^Tree 1.t1t3 \\(5.000, p=1.00\\)\\n"
    )

    # handle singleton case
    res <- process_trees(clades, trees[[1]], verbose = FALSE)
    expect_equal(res[res$Clade == 't1t3',]$Age, c(5))
})


context("Test get_nodeages")
test_that("Test get_nodeages", {

    nh <- get_nodeages(tree)

        expect_equal(nh[nh$node == 6, 'tips'][[1]], 't1, t2, t3, t4, t5')
    expect_equal(nh[nh$node == 6, 'age'][[1]], 19)

    expect_equal(nh[nh$node == 7, 'tips'][[1]], 't4, t5')
    expect_equal(nh[nh$node == 7, 'age'][[1]], 16)

    expect_equal(nh[nh$node == 8, 'tips'][[1]], 't1, t2, t3')
    expect_equal(nh[nh$node == 8, 'age'][[1]], 11)

    expect_equal(nh[nh$node == 9, 'tips'][[1]], 't1, t3')
    expect_equal(nh[nh$node == 9, 'age'][[1]], 5)
})


context("Test get_nodeages (ladderized tree)")
test_that("Test get_nodeages (ladderized tree)", {

    nh <- get_nodeages(ape::ladderize(tree))

    expect_equal(nh[nh$node == 6, 'tips'][[1]], 't1, t2, t3, t4, t5')
    expect_equal(nh[nh$node == 6, 'age'][[1]], 19)

    expect_equal(nh[nh$node == 7, 'tips'][[1]], 't4, t5')
    expect_equal(nh[nh$node == 7, 'age'][[1]], 16)

    expect_equal(nh[nh$node == 8, 'tips'][[1]], 't1, t2, t3')
    expect_equal(nh[nh$node == 8, 'age'][[1]], 11)

    expect_equal(nh[nh$node == 9, 'tips'][[1]], 't1, t3')
    expect_equal(nh[nh$node == 9, 'age'][[1]], 5)
})