#' Rescales a tree
#'
#' @param tree the phylogeny (treeio:treedata, ape::phylo, or ape::multiPhylo)
#' @param scaler a numeric value to rescale tree by
#' @return a phylogeny or a tree
#' @export
#' @examples
#' tree <- rescale_tree(ape::rtree(4), scaler=10)
rescale_tree <- function(tree, scaler) {
    if (inherits(tree, 'phylo')) {
        tree$edge.length <- tree$edge.length * scaler
    } else if (inherits(tree, 'multiPhylo')) {
        tree <- lapply(tree, function(t) rescale_tree(t, scaler))
        class(tree) <- 'multiPhylo'
    } else if (inherits(tree, 'treedata')) {
        tree@phylo$edge.length <- tree@phylo$edge.length * scaler
        for (col in colnames(tree@data)) {
            for (prefix in c("height", "rate", "length")) {
                if (startsWith(col, prefix)) {
                    if (length(tree@data[[col]][[1]]) == 1) {
                        # single values
                        tree@data[[col]] <- rescale(prefix, scaler, as.numeric(tree@data[[col]]))
                    } else {
                        # multiple values (e.g. _range columns)
                        tree@data[[col]] <- lapply(tree@data[[col]], function(x) rescale(prefix, scaler, as.numeric(x)))
                    }
                }
            }
        }
    } else {
        stop(sprintf("Unhandled tree type '%s'", class(tree)))
    }
    tree
}


rescale <- function(coltype, scaler, values) {
    if (coltype == 'rate') {
        values <- values / scaler
    } else {
        values <- values * scaler
    }
    values
}