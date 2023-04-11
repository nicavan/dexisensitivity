#' Title
#'
#' return aggregated attributes ordered by depth. Used for AS
#'
#' @param aTree a subTree
#'
#' @return
#' @export
#'
#' @examples
depth_order <- function(aTree) {
    dtest <- data.frame(name = aTree@Aggregated,
                        Depth = NA)

    for(i in 1:dim(dtest)[1]) {
        tmpname <- dtest[i, "name"]
        tmpID <- getID(aTree@Nodes, tmpname)

        if(length(tmpID) > 1) {
            vecdepth <- NULL
            for(j in tmpID) {
                vecdepth <- c(vecdepth, aTree@Nodes[[j]]@Depth)
            }
            tmpdepth <- max(vecdepth)
        } else {
            tmpdepth <- aTree@Nodes[[tmpID]]@Depth
        }

        dtest[i, "Depth"] <- tmpdepth
    }
    dtest <- dtest[order(dtest$Depth, decreasing = T),]

    return(dtest$name)
}
