#' Perform One-Factor-At-A-Time (OFAT) Sensitivity Analysis
#'
#' Conducts OFAT sensitivity analysis by performing
#' simulations with varying factors, keeping all but one factor constant
#' for each simulation.
#'
#' @param tree A decision tree object to analyze.
#' @param option Initial configuration for tree parameters.
#'
#' @return A matrix with evaluation results for each attribute in the tree,
#' under different parameter variations.
#'
#' @export
oat <- function(tree, option) {
  # Initialize results matrix
  results <- matrix(nrow = tree@NumberOfAttributes,
                    ncol = tree@NumberOfLeaves * 2 + 1)
  rownames(results) <- tree@Attributes
  results[, 1] <- evaluate_scenario(tree, as.matrix(option))

  # Iterate through each leaf and evaluate scenarios with +/- 1 variation
  for (leaf_index in 1:length(tree@Leaves)) {
    # For easier reference
    current_node <- tree@Nodes[[get_id(tree@Nodes, tree@Leaves[leaf_index])[1]]]

    # Evaluate positive variation
    results[, leaf_index * 2] <- evaluate_variation(tree, option,
                                                    leaf_index, 1,
                                                    current_node@RangeScale)

    # Evaluate negative variation
    results[, leaf_index * 2 + 1] <- evaluate_variation(tree, option,
                                                        leaf_index, -1,
                                                        current_node@RangeScale)
  }

  return(results)
}


#' Evaluate Variation for Sensitivity Analysis
#'
#' Helper function to evaluate a specific variation for the sensitivity
#' analysis. It checks both positive and negative variations and returns the
#' evaluation result or -1 if a variation is out of bounds.
#'
#' @param tree A decision tree object to analyze.
#' @param option Configuration to be modified for variation.
#' @param leaf_index Index of the leaf to be varied.
#' @param variation Numeric value indicating the amount and direction of the
#'   variation (+1 or -1).
#' @param range_scale The maximum allowed range for the variation.
#'
#' @return Numeric value indicating the evaluation result of the variation or -1
#'   if out of bounds.
#'
evaluate_variation <- function(tree, option, leaf_index,
                               variation, range_scale) {
  option_copy <- option
  option_copy[leaf_index, ] <- option_copy[leaf_index, ] + variation

  # For positive variation, check if the variation exceeds the range scale
  if (variation > 0 && option_copy[leaf_index, ] > range_scale) {
    return(-1)
  }

  # For negative variation, check if the variation touches the border
  if (variation < 0 && option_copy[leaf_index, ] == 0) {
    return(-1)
  }

  return(evaluate_scenario(tree, as.matrix(option_copy)))
}


#' Visualize One-Factor-At-A-Time (OFAT) Sensitivity Analysis Results
#'
#' Generates a plot to visualize the results of One-Factor-At-A-Time (OFAT)
#' sensitivity analysis.
#'
#' @param nodeName Name of the node for which to generate the plot.
#' @param aResults Matrix of evaluation results returned by the OAT function.
#' @param aTree A decision tree object that was used in the OAT function.
#'
#' @return No return value, but generates a plot.
#'
#' @export
showOAT <- function(nodeName, aResults, aTree) {
    #On récupère l'ID du noeud
    id <- get_id(aTree@Nodes, nodeName)
    # Si on est dans le cas d'une Leaf-Aggregated !
    #   on récupère le noeud aggrégé et non le noeud feuille
    if (aTree@IsLeafAggregated) {
        id <- id %>%
            sapply(function(x) {
                if (!aTree@Nodes[[x]]@IsLeaf) {aTree@Nodes[[x]]@Id}
            }) %>%
            unlist()
    }

    myChildren <- get_leaves(aTree, id)
    list1 <- aResults[nodeName, ]
    nominal <- rep(list1[1], length(myChildren))
    plus <- numeric(length(myChildren))
    minus <- numeric(length(myChildren))

    for(i in 1:length(myChildren)) {
        plus[i] <- list1[2*myChildren[i]]
        minus[i] <- list1[2*myChildren[i] + 1]
    }

    plot(1:length(myChildren), nominal,
         pch = "o", xlab = "", ylab = "Score", main = nodeName, axes = FALSE,
         ylim = c(1, aTree@Nodes[[id]]@RangeScale))
    points(1:length(myChildren), minus, pch = "-")
    points(1:length(myChildren), plus, pch = "+")
    axis(side = 1, at = 1:length(myChildren),
         labels = abbreviate(sapply(1:length(myChildren),
                                    function(x) {aTree@Nodes[[myChildren[x]]]@Name})),
         las = 2)
    axis(side = 2, at = c(1:aTree@Nodes[[id]]@RangeScale),
         labels = 1:aTree@Nodes[[id]]@RangeScale)
}
