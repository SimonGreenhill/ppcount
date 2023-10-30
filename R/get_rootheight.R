#' Returns the rootheight of a tree
#'
#' @param tree the phylogeny (ape::phylo or treeio::treedata)
#' @return a numeric
#' @export
#' @examples
#' get_rootheight(ape::rcoal(4))
get_rootheight <- function(tree) {
    if (class(tree) == 'treedata') { tree <- tree@phylo }
    as.vector(max(diag(ape::vcv.phylo(tree))))
}