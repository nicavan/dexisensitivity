#' OFAT Sensitivity Analysis
#'
#' Executes a One-Factor-At-A-Time (OFAT) sensitivity assessment by undertaking
#' simulations while varying individual factors. During each simulation, all
#' factors are maintained constant save for one.
#'
#' @param tree \code{Tree} object designated for analysis.
#' @param option Initial configuration for \code{Tree} parameters.
#'
#' @return A \code{matrix} depicting the assessment outcomes for every attribute
#'   in the \code{Tree}, corresponding to diverse parameter alterations.
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


#' positive and negative deviations. It yields the evaluation outcome or returns
#' -1 if the deviation is not within the allowed range.
#'
#' @param tree A \code{Tree} object designated for assessment.
#' @param option Configuration set for potential alteration.
#' @param leaf_index \code{numeric} specifying the index of the \code{Node}
#'   (leaf) to be varied.
#' @param variation \code{numeric} value detailing the magnitude and course of
#'   the deviation (+1 or -1).
#' @param range_scale \code{numeric} indicating the maximum permissible scope
#'   for the variation.
#'
#' @return A \code{numeric} value representing the assessment result of the
#'   variation, or -1 if the deviation surpasses the set boundary.
#'
#' @keywords internal
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


#' Visualize OFAT Sensitivity Analysis Outcomes
#'
#' Renders a visualization representing the One-Factor-At-A-Time (OFAT)
#' sensitivity analysis results.
#'
#' @param node_name \code{character} specifying the name of the node intended
#'   for visualization.
#' @param results Matrix of evaluation results, typically derived from
#'   `ofat_sensitivity_analysis`.
#' @param tree A \code{Tree} object that was employed in the OFAT analysis.
#'
#' @return This function does not return a value; instead, it exhibits a plot.
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
