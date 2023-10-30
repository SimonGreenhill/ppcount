#' Rescales a tree
#'
#' @param tree the phylogeny (treeio )
#' @return a phylogeny or a tree
#' @export
#' @examples
#' tree <- rescale_tree(ape::rtree(4), scaler=10)
rescale_tree <- function(tree, scaler) {
    if (class(tree) == 'phylo') {
        tree$edge.length <- tree$edge.length * scaler
    } else if (class(tree) == 'treedata') {
        tree@phylo$edge.length <- tree@phylo$edge.length * scaler
        for (col in colnames(tree@data)) {
            for (prefix in c("height", "rate", "length")) {
                if (startsWith(col, prefix)) {
                    if (length(tree@data[[col]][[1]]) == 1) {
                        # single values
                        tree@data[[col]] <- as.numeric(tree@data[[col]]) * scaler
                    } else {
                        # multiple values (e.g. _range columns)
                        tree@data[[col]] <- lapply(tree@data[[col]], function(x) as.numeric(x) * scaler)
                    }
                }
            }
        }
    } else {
        stop(sprintf("Unhandled tree type '%s'", class(tree)))
    }
    tree
}
