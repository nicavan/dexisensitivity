#' Compute Sensitivity Index (SI) for Decision Tree
#'
#' Computes the Sensitivity Index (SI) for each node within a specified decision
#' tree.
#'
#' @param tree A \code{Tree} object on which the analysis will be executed.
#' @param file_name \code{character} designating the filename to store the SI.
#'   Default is "SI_out.csv".
#' @param is_file \code{logical} determining whether to save the SI results to a
#'   file. Defaults to FALSE
#' @param avoid_repetition \code{logical} indicating if repeated nodes should be
#'   disregarded. Defaults to FALSE.
#'
#' @return A list containing the Sensitivity Indices for every node in the tree.
#'
#' @examples
#' tree <- dexisensitivity::masc2
#' sensitivity_index <- si_dexi(tree)
#' sensitivity_index
#'
#' @export
si_dexi <- function(tree,
                    file_name = "SI_out.csv",
                    is_file = FALSE,
                    avoid_repetition = FALSE) {
  # Initialize SI list for each aggregated node in the decision tree
  si <- vector(mode = "list", length = length(tree@Aggregated))
  names(si) <- tree@Aggregated

  # Calculate SI for each aggregated node
  for (node_name in tree@Aggregated) {
    # Generate a subtree based on the current node and avoidance settings
    sub_tree <- create_sub_tree(tree, node_name,
      avoid_repetition = avoid_repetition
    )

    # Prepare a matrix to store leaf status and depth information for each
    # attribute
    leaf_depth_info <- matrix(nrow = sub_tree@NumberOfAttributes - 1, ncol = 2)

    # Populate the leaf status and depth information
    for (j in 2:sub_tree@NumberOfAttributes) {
      leaf_depth_info[j - 1, 1] <- sub_tree@Nodes[[j]]@IsLeaf
      leaf_depth_info[j - 1, 2] <- sub_tree@Nodes[[j]]@Depth - sub_tree@Nodes[[1]]@Depth + 1
    }

    # Calculate the Sensitivity Index for the subtree using the external
    # function `calculate_sensitivity_index`
    calculated_si <- get_sensitivity_index(sub_tree, avoid_repetition = avoid_repetition)

    # Merge Sensitivity Index results with leaf and depth data, then store in
    # the main SI list
    si[[node_name]] <- matrix(
      c(
        calculated_si[c(sub_tree@Attributes[-1])],
        leaf_depth_info
      ),
      byrow = FALSE,
      ncol = 3,
      dimnames = list(
        c(sub_tree@Attributes[-1]),
        c("SI", "Leaf", "Depth")
      )
    )
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
        col.names = FALSE
      )

      # Next, append the corresponding SI values for that node
      write.table(si[[i]],
        file = file_name,
        append = TRUE,
        sep = ",",
        row.names = TRUE,
        col.names = FALSE
      )
    }
  }

  return(si)
}


#' Compute Sensitivity Index for Decision Tree
#'
#' Computes the Sensitivity Index (SI) for each attribute within the specified
#' tree.
#'
#' @param tree A \code{Tree} object on which the analysis is to be performed.
#' @param avoid_repetition \code{logical} indicating if repeated nodes should be
#'   disregarded. Defaults to FALSE.
#'
#' @return A \code{vector} containing the Sensitivity Indices for each attribute
#'   in the tree.
get_sensitivity_index <- function(tree, avoid_repetition = FALSE) {
  # Initialize root name and attribute weights
  root_name <- tree@RootName
  attribute_weights <- vector(mode = "list", length = tree@NumberOfAttributes)
  names(attribute_weights) <- tree@Attributes

  # Populate the weights from the nodes
  for (index in 1:tree@NumberOfAttributes) {
    attribute_weights[[index]] <- tree@Nodes[[index]]@Probability
  }

  # Prepare to store conditional probabilities
  conditional_probabilities <- vector(
    mode = "list",
    length = tree@NumberOfAttributes
  )
  names(conditional_probabilities) <- tree@Attributes

  # Determine the order to process nodes based on their depth in the tree
  ordered_depth <- depth_order(tree)

  # Process each node in reverse depth order
  for (node_name in ordered_depth) {
    # Get the ID of the node
    node_id <- get_node_id(tree, node_name, avoid_repetition)
    node <- tree@Nodes[[node_id]]

    # Get weights of the node's children for direct descendant calculations
    child_weights <- vector(mode = "list", length = length(node@Children))
    for (i in seq(node@Children)) {
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
    conditional_probabilities[[node_name]] <- c(
      direct_cond_probs,
      indirect_cond_probs
    )
  }

  combined_probs <- c(
    conditional_probabilities[[root_name]],
    attribute_weights[root_name]
  )
  weights <- attribute_weights[names(conditional_probabilities[[root_name]])]
  sensitivity_index <- calculate_sensitivity_indices(
    conditional_prob_list = combined_probs,
    weight_list = weights
  )

  return(sensitivity_index)
}


#' Retrieve Node Identifier by Name from Decision Tree
#'
#' Fetches the unique identifier of a specific node within the provided decision
#' tree based on the node's name. Takes into account potential name repetitions
#' and the hierarchical structure of the tree.
#'
#' @param tree A \code{Tree} object containing the decision tree structure.
#' @param node_name \code{character} specifying the name of the node for which
#'   the ID is sought.
#' @param avoid_repetition \code{logical} indicating whether to return a single
#'   ID in cases of node name repetition. Defaults to FALSE.
#'
#' @return A \code{numeric} representing the identifier of the specified node
#'   within the tree.
#'
#' @noRd

get_node_id <- function(tree, node_name, avoid_repetition) {
  # Start by finding the ID using the node name
  id_finded <- get_id(tree@Nodes, node_name)

  # If no traitment needed, it'll be the same as id_finded
  id <- id_finded

  # If multiple nodes have the same name, filter out the leaf nodes
  if (length(id_finded) > 1) {
    id <- sapply(id_finded, function(x) {
      if (!tree@Nodes[[x]]@IsLeaf) {
        x
      }
    }) |> unlist()
  }

  # If repetitions are to be avoided, only return the first ID
  if (length(id) > 1 & avoid_repetition) {
    id <- id[1]
  }

  # If none of the ID are unique leafs, select the first one
  if (length(id_finded) > 1 & is.null(id)) {
    id <- id_finded[1]
  }


  return(id)
}


#' Direct Probability Computation for Node's Children
#'
#' Calculates the direct probabilities associated with the children of a given
#' node. This computation is based on the aggregation property of the node and
#' the weights assigned to its children.
#'
#' @param node A \code{Node} object representing the specific node for which
#'   direct probabilities are to be computed.
#' @param child_weights A \code{numeric} vector containing the weights of each
#'   child node associated with the provided parent node.
#'
#' @return A \code{list} containing the direct probabilities computed for each
#'   child of the specified node.
#'
#' @noRd
compute_direct_probabilities <- function(node, child_weights) {
  # Compute the direct probabilities based on the node's aggregation type
  probs <- calculate_conditional_probabilities(
    node@Aggregation, child_weights,
    node@RangeScale
  )

  # If the probabilities don't match the node's range scale, adjust them
  if (node@RangeScale != length(probs[[length(node@Children) + 1]])) {
    adjust_probas_for_range_scale(probs, node)
  }

  return(probs)
}


#' Range Scale Probability Adjustment
#'
#' Adjusts a set of probabilities to ensure consistency with a node's specified
#' range scale. If the computed probabilities don't match the node's range
#' scale, they are modified accordingly.
#'
#' @param probs A \code{list} of \code{numeric} vectors representing the
#'   originally computed probabilities for each child of the node.
#' @param node A \code{Node} object specifying the node for which the
#'   probabilities are being adjusted.
#'
#' @return A \code{list} containing the adjusted probabilities for each child of
#'   the node to match the node's range scale.
#'
#' @noRd
adjust_probas_for_range_scale <- function(probs, node) {
  # Calculate the number of children and values in the probabilities
  num_children <- length(node@Children)
  num_values <- length(probs[[num_children + 1]])

  # Make adjustments to ensure the probabilities fit the node's range scale
  probs[[num_children + 1]][c((num_values + 1):node@RangeScale)] <- 0
  names(probs[[num_children + 1]]) <- c(
    names(probs[[num_children + 1]][1:num_values]),
    setdiff(
      as.character(seq(1:node@RangeScale)),
      names(probs[[num_children + 1]])
    )
  )
  return(probs)
}


#' Compute Indirect Probabilities
#'
#' Computes the indirect probabilities for descendants of a specific node,
#' considering the computed probabilities of the node, the existing conditional
#' probabilities, and the structure of the decision tree. If a child of the node
#' is not a leaf, its indirect probabilities are calculated based on its direct
#' ancestors' probabilities.
#'
#' @param node The \code{Node} object for which the indirect probabilities are
#'   calculated.
#' @param probs A \code{list} of computed probabilities for the node.
#' @param conditional_probabilities A \code{list} of existing conditional
#'   probabilities associated with the node's descendants.
#' @param tree The \code{Tree} object representing the decision tree structure.
#' @param avoid_repetition A \code{logical} flag determining if nodes with
#'   repeated names should be avoided in the calculations.
#'
#' @return A \code{list} of computed indirect probabilities for the node's
#'   descendants.
#'
#' @noRd
compute_indirect_probabilities <- function(node, probs,
                                           conditional_probabilities,
                                           tree, avoid_repetition) {
  # Initialize a list to store the indirect probabilities
  indirect_probs <- vector(mode = "list", length = 0)

  # Calculate the indirect probabilities for each child of the node
  for (i in seq(node@Children)) {
    child_id <- get_node_id(tree, node@Children[[i]], avoid_repetition)
    child <- tree@Nodes[[child_id]]
    if (!child@IsLeaf) {
      direct_ancestor_probs <- conditional_probabilities[[node@Children[[i]]]]
      descendant_probs <- vector(
        mode = "list",
        length = length(direct_ancestor_probs)
      )
      for (j in seq(direct_ancestor_probs)) {
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
#' Determines the probabilities of \( Y \) given its direct descendants \( A_i
#' \).
#'
#' @param input_table A \code{matrix} containing all level combinations of the
#'   \( A_i \) factors with the associated \( Y \) values in the last column.
#' @param weight_list A \code{list} with the weight vectors for each \( A_i \)
#'   variable. If missing, levels of \( A_i \) have equal weights.
#' @param sy A \code{numeric} indicating the number of unique \( Y \) values.
#' @param y_levels Optionally, a \code{vector} specifying the \( Y \) levels. If
#'   missing, they are extracted from the \code{input_table}.
#'
#' @return A \code{list} comprising matrices of \( Y \) probabilities
#'   conditional to each \( A_i \) factor. The last element is the vector of
#'   marginal \( Y \) probabilities.
#'
#' @noRd
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
  s_levels <- apply(a_matrix, 2, function(x) {
    length(unique(x))
  })

  if (missing(y_levels)) {
    y_levels <- sort(unique(y_vector))
  }

  # Assign equal weights if weight_list is missing
  if (missing(weight_list)) {
    weight_list <- lapply(s_levels, function(n) {
      rep(1, n) / n
    })
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


#' Calculate Sensitivity Indices (SI) From Conditional Probabilities
#'
#' Computes the first-order sensitivity indices of \( Y \) with respect to \(
#' A_i \) descendants, utilizing the provided conditional probabilities and \(
#' A_i \) weights.
#'
#' @param conditional_prob_list A list containing matrices of conditional
#'   probabilities (of \( Y \) conditional on each \( A_i \)) and a vector of \(
#'   Y \) probabilities as the last element.
#' @param weight_list A list representing the weights associated with each \(
#'   A_i \) factor level.
#'
#' @return A named vector of Sensitivity Indices (SI) where names correspond to
#'   the factors.
#'
#' @noRd
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
#' Visualizes the Sensitivity Index (SI) of the leaves of a specified decision
#' tree using a bar plot.
#'
#' @param tree \code{Tree} object for analysis.
#' @param sensitivity_indices A \code{numeric} vector representing the
#'   Sensitivity Indices for each leaf in the decision tree.
#'
#' @return No return value; a bar plot is displayed.
#'
#' @importFrom graphics mtext
#'
#' @examples
#' tree <- dexisensitivity::masc2
#' sensitivity_index <- si_dexi(tree)
#' plot_sensitivity_index(tree, sensitivity_index[[1]])
#'
#' @export
plot_sensitivity_index <- function(tree, sensitivity_indices) {
  # Ajuster les marges
  old_par <- par(mar = c(5.1, 12.1, 4.1, 2.1))

  # Bar plot parameters and plot generation
  mc <- barplot(
    as.vector(rev(sensitivity_indices[tree@Leaves, 1])),
    horiz = TRUE,
    xlim = c(0, max(sensitivity_indices[tree@Leaves, 1])),
    cex.axis = 0.7 # Réduire la taille de la police
  )
  # Axis modification
  axis(
    side = 2,
    at = mc,
    labels = rev(tree@Leaves),
    las = 2,
    cex.axis = 0.7 # Réduire la taille de la police
  )
  # Reference line
  abline(v = 0.02, untf = FALSE, lty = 3)
  # Additional text on the plot
  mtext("Sensitity Index", 1, line = 3)
  mtext("Basic attributes", 2, outer = TRUE, line = 15)

  # Restaurer les paramètres par défaut
  par(old_par)
}

