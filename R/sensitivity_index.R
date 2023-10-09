#' Calculate Sensitivity Index (SI)
#'
#' Calculates the Sensitivity Index (SI) for a given decision tree.
#'
#' @param tree A decision tree object to perform the analysis on.
#' @param file_name The file name to write the SI to. Default is "SI_out.csv".
#' @param is_file A boolean to decide whether to write the SI to a file. Default
#'   is TRUE.
#' @param avoid_repetition A boolean to decide whether to avoid repeated nodes.
#'   Default is FALSE.
#'
#' @return A list of Sensitivity Indices for each node in the tree.
#'
#' @export
si_dexi <- function(tree,
                    file_name = "SI_out.csv",
                    is_file = TRUE,
                    avoid_repetition = FALSE) {

  # Initialize SI list for each aggregated node in the decision tree
  si <- vector(mode = "list", length = length(tree@Aggregated))
  names(si) <- tree@Aggregated

  # Calculate SI for each aggregated node
  for (node_name in tree@Aggregated) {
    # Generate a subtree based on the current node and avoidance settings
    sub_tree <- create_sub_tree(tree, node_name,
                                avoid_repetition = avoid_repetition)

    # Prepare a matrix to store leaf status and depth information for each
    # attribute
    leaf_depth_info <- matrix(nrow = sub_tree@NumberOfAttributes - 1, ncol = 2)

    # Populate the leaf status and depth information
    for (j in 2:sub_tree@NumberOfAttributes) {
      leaf_depth_info[j-1, 1] <- sub_tree@Nodes[[j]]@IsLeaf
      leaf_depth_info[j-1, 2] <- sub_tree@Nodes[[j]]@Depth - sub_tree@Nodes[[1]]@Depth + 1
    }

    # Calculate the Sensitivity Index for the subtree using the external
    # function `calculate_sensitivity_index`
    calculated_si <- get_sensitivity_index(sub_tree, avoid_repetition = avoid_repetition)

    # Merge Sensitivity Index results with leaf and depth data, then store in
    # the main SI list
    si[[node_name]] <- matrix(c(calculated_si[c(sub_tree@Attributes[-1])],
                                leaf_depth_info),
                              byrow = FALSE,
                              ncol = 3,
                              dimnames = list(c(sub_tree@Attributes[-1]),
                                              c("SI", "Leaf", "Depth")))
  }

  # Write the SI values to a file if required
  if (is_file) {
    for (i in 1:length(tree@Aggregated)) {
      # First, write the node name header
      write.table(names(si)[i],
                  file = file_name,
                  append = TRUE,
                  sep = "",
                  row.names = FALSE,
                  col.names = FALSE)

      # Next, append the corresponding SI values for that node
      write.table(si[[i]],
                  file = file_name,
                  append = TRUE,
                  sep = ",",
                  row.names = TRUE,
                  col.names = FALSE)
    }
  }

  return(si)
}


#' Calculate Sensitivity Index
#'
#' Calculates the Sensitivity Index (SI) of a tree.
#'
#' @param tree The tree object for analysis.
#' @param avoid_repetition A boolean determining if repeated nodes should be
#'   avoided. Default is FALSE.
#'
#' @return A vector of Sensitivity Indices for each attribute in the tree.
get_sensitivity_index <- function(tree, avoid_repetition = FALSE) {

  # Initialize root name and attribute weights
  root_name <- tree@RootName
  attribute_weights <- vector(mode = "list", length = tree@NumberOfAttributes)
  names(attribute_weights) <- tree@Attributes

  # Populate the weights from the nodes
  for(index in 1:tree@NumberOfAttributes) {
    attribute_weights[[index]] <- tree@Nodes[[index]]@Probability
  }

  # Prepare to store conditional probabilities
  conditional_probabilities <- vector(mode = "list",
                                      length = tree@NumberOfAttributes)
  names(conditional_probabilities) <- tree@Attributes

  # Determine the order to process nodes based on their depth in the tree
  ordered_depth <- depth_order(tree)

  # Process each node in reverse depth order
  for(node_name in ordered_depth) {
    # Get the ID of the node
    node_id <- get_node_id(tree, node_name, avoid_repetition)
    node <- tree@Nodes[[node_id]]

    # Get weights of the node's children for direct descendant calculations
    child_weights <- vector(mode = "list", length = length(node@Children))
    for(i in seq(node@Children)) {
      child_weights[[i]] <- attribute_weights[[node@Children[[i]]]]
    }

    # Calculate direct descendant conditional probabilities
    computed_probabilities <- compute_direct_probabilities(node, child_weights)

    names(computed_probabilities) <- c(node@Children, node_name)
    node_weights <- computed_probabilities[[length(computed_probabilities)]]
    direct_cond_probs <- computed_probabilities[-length(computed_probabilities)]

    # Calculate indirect descendant conditional probabilities
    indirect_cond_probs <- compute_indirect_probabilities(node, computed_probabilities, conditional_probabilities, tree, avoid_repetition)

    attribute_weights[[node_name]] <- node_weights
    conditional_probabilities[[node_name]] <- c(direct_cond_probs,
                                                indirect_cond_probs)
  }

  combined_probs <- c(conditional_probabilities[[root_name]],
                      attribute_weights[root_name])
  weights <- attribute_weights[names(conditional_probabilities[[root_name]])]
  sensitivity_index <- calculate_sensitivity_indices(conditional_prob_list = combined_probs,
                                                     weight_list = weights)

  return(sensitivity_index)
}


#' Retrieve Node ID based on Node Name
#'
#' Retrieves the ID of a specific node from the decision tree based on its name,
#' considering possible repetitions and the tree structure.
#'
#' @param tree The tree object
#' @param node_name The name of the node to find.
#' @param avoid_repetition A boolean determining if repeated nodes should be
#'   avoided.
#'
#' @return A numeric value representing the ID of the node.
get_node_id <- function(tree, node_name, avoid_repetition) {
  # Start by finding the ID using the node name
  id <- get_id(tree@Nodes, node_name)

  # If multiple nodes have the same name, filter out the leaf nodes
  if (length(id) > 1) {
    id <- sapply(id, function(x) {
      if (!tree@Nodes[[x]]@IsLeaf) {x}
    }) %>% unlist()
  }

  # If repetitions are to be avoided, only return the first ID
  if (length(id) > 1 & avoid_repetition) {
    id <- id[1]
  }

  return(id)
}


#' Compute Direct Probabilities
#'
#' Computes the direct probabilities for children of a node based on its
#' aggregation and child weights.
#'
#' @param node The specific node for which the direct probabilities should be
#'   calculated.
#' @param child_weights The weights of the child nodes.
#'
#' @return A list of computed direct probabilities.
compute_direct_probabilities <- function(node, child_weights) {
  # Compute the direct probabilities based on the node's aggregation type
  probs <- calculate_conditional_probabilities(node@Aggregation, child_weights,
                                               node@RangeScale)

  # If the probabilities don't match the node's range scale, adjust them
  if (node@RangeScale != length(probs[[length(node@Children) + 1]])) {
    adjust_probas_for_range_scale(probs, node)
  }

  return(probs)
}


#' Adjust Probabilities for Range Scale
#'
#' Adjusts the computed probabilities to match the range scale of a node.
#'
#' @param probs The computed probabilities.
#' @param node The specific node for which the adjustment should be made.
#'
#' @return A list of adjusted probabilities.
adjust_probas_for_range_scale <- function(probs, node) {
  # Calculate the number of children and values in the probabilities
  num_children <- length(node@Children)
  num_values <- length(probs[[num_children + 1]])

  # Make adjustments to ensure the probabilities fit the node's range scale
  probs[[num_children + 1]][c((num_values + 1):node@RangeScale)] <- 0
  names(probs[[num_children + 1]]) <- c(names(probs[[num_children + 1]][1:num_values]),
                                        setdiff(as.character(seq(1:node@RangeScale)),
                                                names(probs[[num_children + 1]])))
  return(probs)
}


#' Compute Indirect Probabilities
#'
#' Computes the indirect probabilities for descendants of a node based on its
#' children, computed probabilities, and the existing conditional probabilities.
#'
#' @param node The specific node for which the indirect probabilities should be
#'   calculated.
#' @param probs The computed probabilities for the node.
#' @param conditional_probabilities The existing conditional probabilities.
#' @param tree A decision tree object.
#' @param avoid_repetition A boolean determining if repeated nodes should be
#'   avoided.
#'
#' @return A list of computed indirect probabilities.
compute_indirect_probabilities <- function(node, probs,
                                           conditional_probabilities,
                                           tree, avoid_repetition) {
  # Initialize a list to store the indirect probabilities
  indirect_probs <- vector(mode = "list", length = 0)

  # Calculate the indirect probabilities for each child of the node
  for(i in seq(node@Children)) {
    child_id  <- get_node_id(tree, node@Children[[i]], avoid_repetition)
    child <- tree@Nodes[[child_id]]
    if (!child@IsLeaf) {
      direct_ancestor_probs <- conditional_probabilities[[node@Children[[i]]]]
      descendant_probs <- vector(mode = "list",
                                 length = length(direct_ancestor_probs))
      for(j in seq(direct_ancestor_probs)) {
        descendant_probs[[j]] <- direct_ancestor_probs[[j]] %*% probs[[node@Children[[i]]]]
      }
      names(descendant_probs) <- names(direct_ancestor_probs)
      indirect_probs <- c(indirect_probs, descendant_probs)
    }
  }

  return(indirect_probs)
}


#' Calculate Conditional Probabilities
#'
#' Calculates the probabilities of Y given its direct descendants A_i.
#'
#' @param input_table A matrix with all the level combinations of the A_i factors
#'   and, in the last column, the associated Y values.
#' @param weight_list A list with the weight vectors of each A_i variable.
#'   If missing, A_i levels have equal weights.
#' @param sy The number of unique Y values.
#' @param y_levels Optional, specifies the Y levels. If missing, they are
#'   extracted from the input_table.
#'
#' @return A list containing matrices of Y probabilities conditional to each
#'   A_i factor and, finally, the vector of marginal Y probabilities.
calculate_conditional_probabilities <- function(input_table,
                                                weight_list,
                                                sy,
                                                y_levels) {
  # Dev Note : no refactoring for this function to keep the same structure as
  # in JEB's scripts. Just added documentation.

  # Preliminary calculations
  a_matrix <- input_table[, -ncol(input_table), drop = F]
  y_vector <- input_table[, ncol(input_table)]

  # Number of factors A (num_factors)
  # and numbers of levels of the factors A (s_levels) and of Y (sy)
  num_factors <- ncol(a_matrix)
  s_levels <- apply(a_matrix, 2, function(x) { length(unique(x)) })

  if (missing(y_levels)) {
    y_levels <- sort(unique(y_vector))
  }

  # Assign equal weights if weight_list is missing
  if (missing(weight_list)) {
    weight_list <- lapply(s_levels, function(n) { rep(1, n) / n })
  }

  # Weights of the table rows for each A variable and for Y
  a_weights <- matrix(NA, nrow(a_matrix), num_factors)
  for (i in 1:num_factors) {
    a_weights[, i] <- weight_list[[i]][a_matrix[, i]]
  }

  y_weights <- apply(a_weights, 1, prod)

  # Calculate the Y probabilities
  y_proba <- c(tapply(y_weights, y_vector, sum))

  # Calculate the Y probabilities conditional to the A_i
  ya_proba_list <- vector("list", length = num_factors)
  for (i in 1:num_factors) {
    probas_i <- y_weights / a_weights[, i]
    cond_proba <- tapply(probas_i, list(a_matrix[, i], y_vector), sum)
    cond_proba[is.na(cond_proba)] <- 0
    ya_proba_list[[i]] <- cond_proba
  }

  # Results storage
  result_list <- c(ya_proba_list, list(y_proba))
  names(result_list) <- colnames(input_table)

  # Modification if sy > number of unique Y values
  if (sy > length(unique(y_vector))) {
    adjusted_list <- vector(mode = "list", length = num_factors + 1)

    for (j in 1:num_factors) {
      adjusted_list[[j]] <- matrix(0, nrow = dim(result_list[[j]])[1], ncol = sy)
      dimnames(adjusted_list[[j]])[1] <- c(dimnames(result_list[[j]])[1])
      dimnames(adjusted_list[[j]])[2] <- list(c(1:sy))

      for (k in as.numeric(dimnames(result_list[[j]])[[2]])) {
        adjusted_list[[j]][, k] <- result_list[[j]][, as.character(k)]
      }
    }

    adjusted_list[[num_factors + 1]] <- rep(0, sy)
    names(adjusted_list[[num_factors + 1]]) <- c(1:sy)

    for (k in as.numeric(names(y_proba))) {
      adjusted_list[[num_factors + 1]][k] <- y_proba[as.character(k)]
    }

    names(adjusted_list) <- colnames(input_table)
    return(adjusted_list)
  }

  return(result_list)
}


#' Calculate Sensitivity indices (SI) From Conditional Probabilities
#'
#' Calculates the first-order sensitivity indices of Y concerning A_i descendants,
#' given the conditional probabilities and A_i weights.
#'
#' @param conditional_prob_list List of matrices of conditional probabilities
#'   (Y conditional to each A_i) plus the vector of Y probabilities.
#' @param weight_list The list of weights for the A_i factor levels.
#'
#' @return A vector of Sensitivity Indices (SI).
calculate_sensitivity_indices <- function(conditional_prob_list, weight_list) {
  # Preliminary steps
  num_factors <- length(conditional_prob_list) - 1
  y_proba <- conditional_prob_list[[num_factors + 1]]
  y_levels <- as.numeric(names(y_proba))

  # Calculate Y expectation and variance
  y_exp <- sum(y_proba * y_levels)
  y_var <- sum(y_proba * (y_levels - y_exp)^2)

  # Calculate sensitivity indices
  si_indices <- vector(length = num_factors)
  for (i in seq(num_factors)) {
    y_cond_exp_ai <- conditional_prob_list[[i]] %*% y_levels
    y_var_ai <- sum(weight_list[[i]] * (y_cond_exp_ai - y_exp)^2)
    si_indices[i] <- y_var_ai / y_var
  }

  # Store results
  names(si_indices) <- names(conditional_prob_list[seq(num_factors)])
  return(si_indices)
}



#' Show Sensitivity Index (SI)
#'
#' Generates a bar plot to visualize the Sensitivity Index (SI) of the leaves of
#' a given decision tree.
#'
#' @param tree A decision tree object for analysis.
#' @param sensitivity_indices A vector of Sensitivity Indices for each leaf
#'   in the decision tree.
#'
#' @return No return value, but generates a bar plot.
#'
#' @importFrom graphics mtext
plot_sensitivity_index <- function(tree, sensitivity_indices) {
  # Bar plot parameters and plot generation
  mc <- barplot(
    as.vector(rev(sensitivity_indices[tree@Leaves, 1])),
    horiz = TRUE,
    xlim = c(0, max(sensitivity_indices[tree@Leaves, 1])),
    ylab = "Indicators"
  )
  # Axis modification
  axis(
    side = 2,
    at = mc,
    labels = rev(tree@Leaves),
    las = 2
  )
  # Reference line
  abline(v = 0.02, untf = FALSE, lty = 3)
  # Additional text on the plot
  mtext("Sensitity Index", 1, line = 3)
  mtext("Basic attributes", 2, outer = TRUE, line = 15)
}
