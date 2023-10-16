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
  results <- matrix(
    nrow = tree@NumberOfAttributes,
    ncol = tree@NumberOfLeaves * 2 + 1
  )
  rownames(results) <- tree@Attributes
  results[, 1] <- evaluate_scenario(tree, as.matrix(option))

  # Iterate through each leaf and evaluate scenarios with +/- 1 variation
  for (leaf_index in 1:length(tree@Leaves)) {
    # For easier reference
    current_node <- tree@Nodes[[get_id(tree@Nodes, tree@Leaves[leaf_index])[1]]]

    # Evaluate positive variation
    results[, leaf_index * 2] <- evaluate_variation(
      tree, option,
      leaf_index, 1,
      current_node@RangeScale
    )

    # Evaluate negative variation
    results[, leaf_index * 2 + 1] <- evaluate_variation(
      tree, option,
      leaf_index, -1,
      current_node@RangeScale
    )
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
#' Generates a plot visualizing the results of the OFAT sensitivity analysis.
#'
#' @param node_name Name of the node for the visualization.
#' @param results Evaluation results matrix obtained from `ofat_sensitivity_analysis`.
#' @param tree Decision tree object used in the OFAT analysis.
#'
#' @return No return value; the function displays a plot.
#'
#' @export
show_oat_results <- function(node_name, results, tree) {
  # Retrieve the node ID
  node_id <- get_id(tree@Nodes, node_name)

  # Handle the case of a Leaf-Aggregated node:
  # Retrieve the aggregated node instead of the leaf node
  if (tree@IsLeafAggregated) {
    node_id <- sapply(node_id, function(x) {
      if (!tree@Nodes[[x]]@IsLeaf) tree@Nodes[[x]]@Id
    }) %>% unlist()
  }

  child_nodes <- get_leaves(tree, node_id)
  scores <- results[node_name, ]
  nominal_scores <- rep(scores[1], length(child_nodes))

  # Extract positive and negative variations for each child node
  plus_scores <- sapply(child_nodes, function(x) scores[2 * x])
  minus_scores <- sapply(child_nodes, function(x) scores[2 * x + 1])

  # Create the plot
  plot(1:length(child_nodes), nominal_scores,
    pch = "o", xlab = "", ylab = "Score", main = node_name, axes = FALSE,
    ylim = c(1, tree@Nodes[[node_id]]@RangeScale)
  )

  # Add negative and positive variations to the plot
  points(1:length(child_nodes), minus_scores, pch = "-")
  points(1:length(child_nodes), plus_scores, pch = "+")

  # Add axis labels
  axis_labels <- abbreviate(sapply(child_nodes, function(x) tree@Nodes[[x]]@Name))
  axis(side = 1, at = 1:length(child_nodes), labels = axis_labels, las = 2)
  axis(
    side = 2, at = c(1:tree@Nodes[[node_id]]@RangeScale),
    labels = 1:tree@Nodes[[node_id]]@RangeScale
  )
}
