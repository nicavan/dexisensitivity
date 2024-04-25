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
  ordered_depth_data <- depth_data[order(depth_data$depth, decreasing = TRUE), ]


  # Correction of evaluation order if aggregated nodes are used as leaves
  if (tree@IsLeafAggregated){
    # Having ordered row numbers is necessary for the following part
    rownames(ordered_depth_data) <- 1:nrow(ordered_depth_data)

    # For each aggregated node used as a leaf somewhere else in the tree
    for (leaf in tree@LeafAggregated){

      id <- get_node_id(tree, leaf, avoid_repetition = F)

      # Check if children of this aggregated criteria are all normal leaves.
      if (sum(tree@Nodes[[id]]@Children %in% tree@Aggregated)==0){
        # In that case, we put the line on top of the depth_order matrix
        ordered_depth_data <- ordered_depth_data[order(ordered_depth_data$name == leaf, decreasing = T), ]

      }else{ #ie at least one other aggregated node is used to define this node
        l <- 1
        # saving id_line and content of the aggregated node
        id_line <- which(ordered_depth_data$name == leaf)

        # check if all children are already processed (normal leaves and other aggregated nodes)
        while (sum(tree@Nodes[[id]]@Children %in%
                   c(tree@Leaves,ordered_depth_data$name[1:l]))!=length(tree@Nodes[[id]]@Children) &&
               l < length(tree@Aggregated)){
          l <- l+1
        }

        # the aggregated node is not well ordered and a better option does exist
        if (l < nrow(ordered_depth_data) && id_line != (l+1)){

          save_data <- ordered_depth_data[ordered_depth_data$name == leaf,]

          # deleting the line of the matrix with the aggregated node used as a leaf
          ordered_depth_data <- ordered_depth_data[ordered_depth_data$name != leaf,]

          c <- 0
          # if aggregated node is before line l, we need to insert it one line earlier in ordered_depth
          if (id_line < l){
            c <- -1
          }

          # Insertion of the aggregated node in the right place in ordered_depth
          ordered_depth_data <- rbind(ordered_depth_data[1:(l+c),],
                                      save_data,
                                      ordered_depth_data[-(1:(l+c)),])

          multiple_ids <- get_id(tree@Nodes,leaf) #we want all ids to check mothers of the node and check their positions
          # list all mothers of this aggregated node used as a leaf
          mothers <- NULL
          for (id in multiple_ids){
            mothers <- c(mothers, tree@Nodes[[id]]@Mother)
          }

          # for each mother, check if ordered after the new position of aggregated leaf
          for (node in mothers){
            id_mother <- which(ordered_depth_data$name == node)
            # Node dev à gérer : effet de bord (position de la mère pas parfaite)
            if (id_mother < (l+c+1)){
              save_data_mother <- ordered_depth_data[ordered_depth_data$name == node,]
              ordered_depth_data <- ordered_depth_data[ordered_depth_data$name != node,]
              ordered_depth_data <- rbind(ordered_depth_data[1:(l+c),],
                                          save_data_mother,
                                          ordered_depth_data[-(1:(l+c)),])

            }
          }
        }
      }
    }
  }

  return(ordered_depth_data$name)
}
