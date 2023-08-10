#' Get Aggregated Attributes Ordered by Depth
#'
#' Return aggregated attributes ordered by depth. This function is typically
#' used for analysis strategy (AS).
#'
#' @param aTree A decision tree object.
#'
#' @return A vector of aggregated attribute names, ordered by depth.
#'
#' @export
depth_order <- function(aTree) {
    dtest <- data.frame(name = aTree@Aggregated,
                        Depth = NA)

    for(i in 1:dim(dtest)[1]) {
        tmpname <- dtest[i, "name"]
        tmpID <- get_id(aTree@Nodes, tmpname)

        if(length(tmpID) > 1) {
            vecdepth <- NULL
            for(j in tmpID) {
                if(!aTree@Nodes[[j]]@IsLeaf) {
                    vecdepth <- c(vecdepth, aTree@Nodes[[j]]@Depth)
                }
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
