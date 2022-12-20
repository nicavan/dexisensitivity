#' Title
#'
#' @param aTree A Tree
#' @param nbOptions Number option to create
#' @param aSeed seed for random number generation
#'
#' @return
#' @export
#'
#' @examples
createOptions <- function(aTree, nbOptions = 1, aSeed = -1) {
    if (aSeed>0) {
        set.seed(aSeed)
    }

    option <- matrix(nrow = aTree@nbLeaves, ncol = nbOptions)
    rownames(option) <- aTree@Leaves

    for(k in aTree@Leaves) {
        option[k, ] <- sample(aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@rangeScale,
                              size = nbOptions,
                              prob = aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@Proba,
                              replace = TRUE)
    }

    return(option)
}

