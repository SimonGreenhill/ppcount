#' Returns a list of tips and nodes in the tree and their relevant ages.
#'
#' @param tree the phylogeny
#' @return A list of Node ages and Tip Ages.
#' ## node.ages is a data frame listing as variables
## the identity of parental and
## daughter nodes, the distance from the root and
## from the present of each node,
## the branch length and the age of the most recent
## common ancestor

#' @note taken from https://grokbase.com/t/r/r-sig-phylo/116m5s3fr4/r-nodes-and-taxa-depth
#' @export
#' @examples
#' get_ages(ape::rtree(4))
get_ages <- function(tree) {
    # see https://grokbase.com/t/r/r-sig-phylo/116m5s3fr4/r-nodes-and-taxa-depth
    tree.age <- picante::node.age(tree)
    BL.position <- cbind(tree.age$edge, tree.age$age, tree$edge.length)
    dist.tip <- max(tree.age$age) - BL.position[,3]
    node.ages <- cbind(BL.position, dist.tip)
    node.ages <- cbind(node.ages, node.ages[,5] + node.ages[,4])
    node.ages <- as.data.frame(node.ages)
    names(node.ages) <- c("parental.node", "daughter.node", "dist.root", "BL", "dist.tip","mrca.age")
    tip.ages <- node.ages[node.ages[, 2] < length(tree.age$tip) + 1, ]
    row.names(tip.ages) <- tree.age$tip
    list(Nodes = node.ages, Tips = tip.ages)
}

get_age_for_clade <- function(tree, ages, clade, type="median") {
    node <- as.character(ape::getMRCA(tree, clade))
    m <- ages$Nodes[ages$Nodes['parental.node']  == node,]$mrca.age
    if (type == 'median') {
        stats::median(m)
    }
    else if (type == 'mean') {
        mean(m)
    }
}


read_clade_file <- function(filename) {
    clades <- list()
    for (line in readLines(filename)) {
        if (substr(line, 1, 6) == 'taxset') {
            line <- substr(line, 8, nchar(line))
            line <- sub(';', '', line)
            line <- strsplit(line, "\\s+=\\s")[[1]]
            clade <- line[[1]]
            taxa <- strsplit(line[[2]], '\\s+')[[1]]
            taxa <- taxa[taxa != ""]
            clades[[clade]] <- taxa
        }
    }
    clades
}

process_trees <- function(clades, trees, verbose=FALSE) {
    df <- NULL
    for (i in 1:length(trees)) {
        tree <- trees[[i]]
        ages <- get_ages(tree)
        for (clade in names(clades)) {
            n <- length(clades[[clade]])
            if (n > 1) {
                m <- ape::is.monophyletic(tree, clades[[clade]])
                a <- get_age_for_clade(tree, ages, clades[[clade]])
            } else {
                m <- 1
                a <- ages$Tips[clades[[clade]][[1]],]$mrca.age
            }
            if (verbose == TRUE) message(paste("Tree", i, clade, a, m))

            if (is.null(df)) {
                df <- tibble::tibble(Tree = i, Clade = clade, Age = a, Monophyly = m)
            } else {
                df <- tibble::add_row(df, Tree = i, Clade = clade, Age = a, Monophyly = m)
            }
        }
    }
    df
}
