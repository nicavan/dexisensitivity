#' Get Aggregated Attributes Ordered by Depth
#'
#' Returns aggregated attributes ordered by depth. Typically
#' used for analysis strategy (AS).
#'
#' @param tree A decision tree object.
#'
#' @return A vector of aggregated attribute names, ordered by depth.
depth_order <- function(tree) {
  # Initialize data frame to store attributes and their depths
  depth_data <- data.frame(name = tree@Aggregated, depth = NA)

  # Iterate through each attribute to determine its depth in the decision tree
  for (i in 1:nrow(depth_data)) {
    attribute_name <- depth_data[i, "name"]
    node_ids <- get_id(tree@Nodes, attribute_name)

    # Check if the attribute is present in multiple nodes
    if (length(node_ids) > 1) {
      depths <- NULL
      # Determine depth for each node where attribute is present
      for (j in node_ids) {
        if (!tree@Nodes[[j]]@IsLeaf) {
          depths <- c(depths, tree@Nodes[[j]]@Depth)
        }
      }
      attribute_depth <- max(depths)
    } else {
      attribute_depth <- tree@Nodes[[node_ids]]@Depth
    }

    # Update depth for the current attribute
    depth_data[i, "depth"] <- attribute_depth
  }

  # Order the attributes by depth in descending order
  ordered_depth_data <- depth_data[order(depth_data$depth, decreasing = TRUE),]

  return(ordered_depth_data$name)
}
