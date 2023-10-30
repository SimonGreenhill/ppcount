#' Returns a table of tips and nodes in the tree and their relevant ages.
#'
#' The columns are:
#'    parental.node - the parent node
#'    daughter.node - the daughter node
#'    dist.root - the distance from the root
#'    BL - this branch length
#'    dist.tip - the distance of the tip.
#'    mrca.age - age of the most recent common ancestor
#' @param tree the phylogeny
#' @return A data frame (as tibble) of Node and Tip Ages.
#' @note taken from https://grokbase.com/t/r/r-sig-phylo/116m5s3fr4/r-nodes-and-taxa-depth
#' @export
#' @examples
#' get_ages(ape::rtree(4))
get_ages <- function(tree) {
    if (class(tree) == 'treedata') { tree <- tree@phylo }
    
    # this fixes the error with ladderized trees see here for more details:
    # https://markmail.org/thread/aiwardimvsej6vby#query:+page:1+mid:hcrazszdj2fo2i7z+state:results
    attr(tree, "order") <- NULL
    tree <- reorder(tree)

    # see https://grokbase.com/t/r/r-sig-phylo/116m5s3fr4/r-nodes-and-taxa-depth
    tree <- picante::node.age(tree)

    BL.position <- cbind(tree$edge, tree$age, tree$edge.length)
    dist.tip <- max(tree$age) - BL.position[,3]
    ages <- cbind(BL.position, dist.tip)
    ages <- cbind(ages, ages[,5] + ages[,4])
    ages <- as.data.frame(ages)
    names(ages) <- c("parental.node", "daughter.node", "dist.root", "BL", "dist.tip","mrca.age")

    ages$Label <- rownames(ages)
    ages[c(ages[, 2] < length(tree$tip) + 1), 'Label'] <- tree$tip.label
    ages <- tibble::as_tibble(ages)
    ages <- tibble::column_to_rownames(ages, 'Label')
    ages
}


#' Extracts the age of a given clade group from the tree.
#' @param tree the phylogeny
#' @param clade the clade label
#' @param ages An age dataframe from `get_ages` (define this to speed up repeated evaluation)
#' @importFrom stats median
#' @return float.
#' @export
#' @examples
#' get_age_for_clade(ape::rtree(5), c('t1', 't3'))
get_age_for_clade <- function(tree, clade, ages=NULL) {
    if (class(tree) == 'treedata') { tree <- tree@phylo }
    
    if (length(setdiff(clade, tree$tip.label)) > 0) {
        stop(
            paste("Invalid taxon in clade:",
            paste(setdiff(clade, tree$tip.label), collapse=", ")
        ))
    }

    if (is.null(ages)) { ages <- get_ages(tree) }
    node <- as.character(ape::getMRCA(tree, clade))
    m <- ages[ages['parental.node']  == node, 'mrca.age']
    stats::median(m)
}


#' Reads a clade file and parses it.
#' The format is:
#'    taxset t1t3 = t1 t3;
#'    taxset t4t5 = t4 t5;
#' @param filename the clade file to read
#' @export
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


#' Extracts a dataframe (as tibble) for all the clades from a given tree (or list
#' of trees).
#' @param clades a list of clades
#' @param trees the phylogeny
#' @param verbose show progress messages or not
#' @return dataframe(tibble)
#' @export
#' @examples
#' tree <- ape::rtree(5)
#' clades <- list(A = c('t1', 't3'), B = c('t1', 't2', 't3'))
#' process_trees(clades, tree)
process_trees <- function(clades, trees, verbose=FALSE) {
    if (class(trees) == 'treedata') { trees <- trees@phylo }
    
    if (class(trees) == 'phylo') {  # handle singleton case
        trees <- list(trees); class(trees) <- 'multiPhylo'
    }

    # check tip labels
    for (clade in names(clades)) {
        if (any(clades[[clade]] %in% trees[[1]]$tip.label == FALSE)) {
            stop("")
        }
    }

    df <- NULL
    for (i in 1:length(trees)) {
        ages <- get_ages(trees[[i]])
        for (clade in names(clades)) {
            n <- length(clades[[clade]])
            if (n > 1) {
                m <- ape::is.monophyletic(trees[[i]], clades[[clade]])
                a <- get_age_for_clade(trees[[i]], clades[[clade]], ages)
            } else {
                m <- 1
                a <- ages[clades[[clade]][[1]], 'mrca.age']
            }
            if (verbose == TRUE) message(sprintf("Tree %d.%s (%0.3f, p=%0.2f)", i, clade, a, m))

            if (is.null(df)) {
                df <- tibble::tibble(Tree = i, Clade = clade, Age = a, Monophyly = m)
            } else {
                df <- tibble::add_row(df, Tree = i, Clade = clade, Age = a, Monophyly = m)
            }
        }
    }
    df
}


#' Returns a tibble of the node ages for the given tree.
#' @param tree the phylogeny
#' @return A data frame (as tibble) of Node Ages.
#' @importFrom stats median
#' @export
#' @examples
#' tree <- ape::read.tree(text = "((t4:1,t5:2):3,((t3:4,t1:5):6,t2:7):8);")
#' get_nodeages(tree)
get_nodeages <- function(tree) {
    if (class(tree) == 'treedata') { tree <- tree@phylo }
    
    ages <- get_ages(tree)
    nodes <- sort(unique(ages$parental.node))
    get_age <- function(a) { median(ages[ages$parental.node == a, 'mrca.age']) }
    get_tips <- function(a) { toString(sort(ape::extract.clade(tree, a)$tip.label)) }
    tibble::tibble(
        node=nodes,
        age=sapply(nodes, get_age),
        tips=sapply(nodes, get_tips)
    )
}
