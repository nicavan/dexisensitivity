#' Generate Random Options Matrix for a Given Tree
#'
#' Creates random options based on the \code{RangeScale} and \code{Probability} attributes
#' of tree nodes. This function is useful for generating random scenarios for simulations
#' or analyses.
#'
#' @param tree A \code{Tree} object.
#' @param num_options A single \code{numeric} value specifying the number of options to generate.
#'   Defaults to 1.
#' @param seed A single \code{numeric} value for random number generation seed.
#'   Default is \code{NULL}, which means no seed will be set.
#'
#' @return A \code{matrix} where rows represent tree leaves and columns represent
#'   sampled options.
#'
#' @details
#' The function creates a matrix of random options based on the attributes of tree nodes.
#' Specifically, it leverages the \code{RangeScale} and \code{Probability} attributes of tree nodes
#' to generate random options.
#'
#' @examples
#' tree <- dexiranalysis::masc2
#' option <- create_options(tree, num_options=3, seed = 42)
#'
#' @export
create_options <- function(tree, num_options = 1, seed = NULL) {
  # Ensure inputs are of the expected type
  if (!inherits(tree, "Tree")) {
    stop("Expected 'tree' to be of class 'Tree'")
  }
  if (!is.numeric(num_options) || length(num_options) != 1) {
    stop("'num_options' should be a single numeric value")
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1)) {
    stop("'seed' should be a single numeric value or NULL")
  }

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Initialize the options matrix
  options_matrix <- matrix(nrow = tree@NumberOfLeaves, ncol = num_options)
  rownames(options_matrix) <- tree@Leaves

  # Populate the matrix with random samples based on node attributes
  for (leaf in tree@Leaves) {
    node_id <- get_id(tree@Nodes, leaf)[1]
    node <- tree@Nodes[[node_id]]

    options_matrix[leaf, ] <- sample(node@RangeScale,
      size = num_options,
      prob = node@Probability,
      replace = TRUE
    )
  }

  return(options_matrix)
}


#' Evaluate Node Values in a Tree
#'
#' Calculates the values of the tree nodes using a bottom-up approach. The function
#' starts by assigning values to the leaves, based on the provided option, and then
#' aggregates these values up the tree to determine each node's value. This ensures
#' each node's value considers the values of its child nodes.
#'
#' @param tree A \code{Tree} object.
#' @param option A matrix representation of a scenario, providing values for the
#'   tree's leaves. Each column in the matrix corresponds to a tree leaf, and the
#'   rows provide different values for the scenario analysis.
#'
#' @return A named \code{numeric} vector representing the evaluated values for all
#'   nodes, progressing from the leaves to the root.
#'
#' @details
#' Begins by assigning values to the leaves of the tree based on the
#' provided option. If the tree structure indicates leaf-aggregated scenarios,
#' these values are aggregated accordingly. The function then continues to aggregate
#' values up the tree, considering the tree's structure, to determine each node's value.
#'
#' It's essential for the input option matrix to have columns that correspond to the
#' leaves of the tree and for the tree object to have the appropriate attributes set.
#'
#' @examples
#' tree <- dexiranalysis::masc2
#' option <- create_options(tree, num_options=1, seed = 42)
#' scenario <- evaluate_scenario(tree, option)
#' scenario
#'
#' @export
evaluate_scenario <- function(tree, option) {
  # Ensure the option is matrix-formatted
  if (!is.matrix(option)) {
    stop("The option must be a matrix.")
  }

  # Set a starting state for results
  results <- numeric(tree@NumberOfAttributes)
  names(results) <- tree@Attributes
  results[] <- -1

  # Assign values to the leaves of the tree based on the provided option
  results <- assign_values_to_leaves(results, option)

  # If the tree has leaf-aggregated scenarios, compute the aggregated values
  if (tree@IsLeafAggregated) {
    results <- compute_aggregated_values(tree, results)
  }

  # Compute the final aggregated values based on the tree structure
  results <- compute_final_aggregated_values(tree, results)

  return(results)
}


#' Assign Values to the Leaves of the Tree
#'
#' Assigns values from the provided option matrix to the corresponding leaf nodes
#' in the tree's results vector. Ensures that there are no Leaf-Aggregated
#' leaves in the given option.
#'
#' @param results A \code{numeric} vector that serves as a placeholder for the
#'   evaluated values.
#' @param option A matrix representing a single option or scenario to evaluate. Each
#'   row in the matrix corresponds to a leaf node, and each column provides different
#'   values for the scenario analysis.
#'
#' @return A \code{numeric} vector with leaf-populated values.
#'
#' @keywords internal
assign_values_to_leaves <- function(results, option) {
  # Assign values from the option matrix to the corresponding leaf in results
  for (i in 1:length(option)) {
    leaf_name <- dimnames(option)[[1]][i]
    results[which(names(results) == leaf_name)] <- option[i]
  }
  return(results)
}


#' Calculate Values for Leaf-Aggregated Scenarios
#'
#' Computes aggregated values for leaf-aggregated scenarios based on the tree's
#' evaluation order. Ensures that values are calculated in the correct
#' sequence to account for aggregation.
#'
#' @param tree A \code{Tree} object.
#' @param results A \code{numeric} vector that serves as a placeholder for the
#'   evaluated values.
#'
#' @return A \code{numeric} vector with computed aggregated values.
#'
#' @keywords internal
compute_aggregated_values <- function(tree, results) {
  # Use the tree's order for aggregating results
  for (i in 1:length(tree@EvaluationOrder)) {
    sub_tree <- create_sub_tree(tree, tree@Attributes[tree@EvaluationOrder[i]])
    results <- compute_values_from_aggregation_table(sub_tree, results)
  }
  return(results)
}


#' Calculate Final Aggregated Values
#'
#' Computes the final aggregated values using the tree's structure. Ensures that
#' values are propagated correctly through the tree.
#'
#' @param tree A \code{Tree} object.
#' @param results A \code{numeric} vector serving as a placeholder for the
#'   evaluated values.
#'
#' @return A \code{numeric} vector with the final computed aggregated values.
#'
#' @keywords internal
compute_final_aggregated_values <- function(tree, results) {
  results <- compute_values_from_aggregation_table(tree, results)

  # Propagate values through the main tree
  for (k in tree@Attributes) {
    gotten_id <- get_id(tree@Nodes, k)
    results[gotten_id] <- max(results[gotten_id])
  }

  return(results)
}


#' Compute Node Values from Aggregation Table
#'
#' Calculates node values iteratively using the aggregation table and related children.
#'
#' @param tree A \code{Tree} object.
#' @param results A \code{numeric} vector with pre-existing values.
#'
#' @return A \code{numeric} vector with updated node values.
#'
#' @keywords internal
compute_values_from_aggregation_table <- function(tree, results) {
  for (agg_nodes_rev in rev(tree@Aggregated)) {
    if (results[agg_nodes_rev] < 0) {
      node_ids <- get_id(tree@Nodes, agg_nodes_rev)

      # Address non-leaf nodes
      if (length(node_ids) > 1) {
        node_ids <- node_ids |>
          sapply(function(x) {
            if (!tree@Nodes[[x]]@IsLeaf) {
              x
            }
          }) |>
          unlist()
      }

      for (node_id in node_ids) {
        num_children <- length(tree@Nodes[[node_id]]@Children)
        aggregation_table <- tree@Nodes[[node_id]]@Aggregation

        # Adjust values based on child nodes and aggregation table
        for (k in 1:num_children) {
          child_value <- results[tree@Nodes[[node_id]]@Children[k]]
          aggregation_table <- aggregation_table[aggregation_table[, k] == child_value, ]
        }

        results[agg_nodes_rev] <- aggregation_table[num_children + 1]
      }
    }
  }

  return(results)
}


#' Evaluate Multiple Scenarios for a Given Tree
#'
#' Evaluates multiple scenarios simultaneously using the \code{evaluate_scenario} function.
#' Each scenario is represented as a column in the \code{options_matrix}.
#'
#' @param tree A \code{Tree} object for evaluation.
#' @param options_matrix A \code{matrix} where each column represents a scenario.
#'
#' @return A \code{list} of \code{numeric} vectors with evaluation results for each scenario.
#'
#' @examples
#' tree <- dexiranalysis::masc2
#' option <- create_options(tree, num_options=3, seed = 42)
#' scenarios <- evaluate_scenarios(tree, option)
#' scenarios
#'
#' @export
evaluate_scenarios <- function(tree, options_matrix) {
  # Using sapply to loop over each column in options_matrix.
  # Each column is treated as a scenario and passed to evaluate_scenario.
  1:dim(options_matrix)[2] |>
    sapply(function(x) {
      evaluate_scenario(tree, as.matrix(options_matrix[, x]))
    })
}


#' Save Options Table
#'
#' Stores a matrix of options into a file, primarily for archival or subsequent
#' analysis.
#'
#' @param options_table A \code{matrix} containing option values.
#' @param file_name A \code{character} string specifying the desired file name for
#'   saving the options.
#'
#' @return NULL
#'
#' @export
save_options <- function(options_table, file_name) {
  # Utilizing utils package to write the options table into a file The choice of
  # tab-separated format ensures readability and compatibility with many
  # applications
  utils::write.table(options_table,
    file = file_name,
    sep = "\t",
    row.names = TRUE,
    col.names = NA,
    quote = FALSE
  )
}


#' Load Options Table from a File
#'
#' Retrieves a matrix of options saved in a file. This matrix can then be used
#' for further analysis or processing.
#'
#' @param file_name A \code{character} string specifying the file from which to
#'   load the options matrix.
#'
#' @return A \code{matrix} representing the loaded options.
#'
#' @export
load_options <- function(file_name) {
  # Using utils package to read the table from the file.
  # The choice of tab-separated format matches our save_options function
  # to ensure consistent data interchange.
  as.matrix(utils::read.table(
    file = file_name,
    header = TRUE,
    sep = "\t",
    row.names = 1
  ))
}


#' Save Evaluation Results of Scenarios to a File
#'
#' Stores the results of scenario evaluations into a file for later analysis.
#'
#' @param scenarios_results List of \code{numeric} vectors with scenario
#'   evaluation results.
#' @param file_name A \code{character}, to name the file for saving the scenario
#'   results.
#'
#' @return NULL
#'
#' @export
save_scenarios <- function(scenarios_results, file_name) {
  # Using the utils package to write the scenario results into a file. The
  # choice of tab-separated format ensures readability and compatibility across
  # many applications.
  utils::write.table(scenarios_results,
    file = file_name,
    sep = "\t",
    row.names = TRUE,
    col.names = NA
  )
}


#' Plot a Bar Chart for a Single Scenario
#'
#' Visualizes the attribute values of a provided scenario. For each attribute, a
#' bar is plotted, and the maximum possible value is highlighted.
#'
#' @param scenario Scenario data to visualize.
#' @param tree Associated \code{Tree} object providing attribute details.
#' @param label_y \code{Logical} value indicating whether to label the Y-axis
#'   (default is \code{TRUE}).
#' @param modify_par \code{Logical} value to decide if graphical parameters
#'   should be modified (default is \code{TRUE}).
#'
#' @return NULL
#'
#' @seealso \code{\link{evaluate_scenario}}
#'
#' @examples
#' tree <- dexiranalysis::masc2
#' option <- create_options(tree, num_options=1, seed = 42)
#' scenario <- evaluate_scenario(tree, option)
#' show_scenario(as.matrix(scenario), tree = tree, label_y = TRUE)
#'
#' @export
show_scenario <- function(scenario, tree, label_y = TRUE, modify_par = TRUE) {
  if (modify_par) {
    old_par <- par(mgp = c(7, 1, 0), oma = c(0, 20, 0, 0), cex = 0.5)
    base::on.exit(par(old_par), add = TRUE)
  }

  # Define the gray scale based on the range scale of attributes
  grey_values <- lapply(1:7, function(x) {
    grDevices::gray.colors(x, 0, 1)
  })
  bar_colors <- tree@Leaves |>
    sapply(function(x) {
      grey_values[[tree@Nodes[[get_id(tree@Nodes, x)[1]]]@RangeScale]][scenario[x, ]]
    }) |>
    unlist()

  max_values <- tree@Attributes |>
    lapply(function(x) {
      tree@Nodes[[get_id(tree@Nodes, x)[1]]]@RangeScale
    }) |>
    unlist() |>
    matrix(ncol = 1)

  # Plot the bars
  mc <- graphics::barplot(as.vector(rev(scenario)),
    xlim = c(0, max(max_values[]) + 0.5),
    ylab = "Indicators",
    xlab = "Mark",
    horiz = TRUE,
    col = rev(bar_colors)
  )

  # Add Y-axis labels if needed
  if (label_y) {
    graphics::axis(
      side = 2,
      at = mc,
      labels = rev(rownames(scenario)),
      las = 2,
      cex = 0.5
    )
  }

  # Mark maximum values
  graphics::points(as.vector(rev(max_values)),
    mc,
    col = "black",
    pch = "<"
  )

  # Add dashed vertical lines for reference
  graphics::abline(v = c(1:max(max_values)), untf = FALSE, lty = 3)
}


#' Compare Scenarios Using a Radial Plot
#'
#' Visualizes the comparison of node values across multiple scenarios with a
#' radial plot. This representation provides an intuitive view of how different
#' scenarios compare for the selected nodes.
#'
#' @param tree A \code{Tree} object.
#' @param scenarios_results List of \code{numeric} vectors with scenario
#'   evaluation results.
#' @param nodes_list List of \code{character} node names to be compared in the
#'   plot.
#'
#' @return NULL
#'
#' @seealso \code{\link{evaluate_scenarios}}
#'
#' @examples
#' tree <- dexiranalysis::masc2
#' option <- create_options(tree, num_options=3, seed = 42)
#' scenarios <- evaluate_scenarios(tree, option)
#' compare_scenarios(tree, scenarios,
#'  c("Dimension economique", "Dimension sociale", "Dimension environnementale"))
#'
#' @importFrom plotrix radial.plot
#' @export
compare_scenarios <- function(tree, scenarios_results, nodes_list) {
  # Adjust the point size for better visualization in the radial plot
  old_par <- par(ps = 6)
  base::on.exit(par(old_par))

  # Find the maximum value across all scenarios and nodes for setting the radial limit
  max_value <- max(scenarios_results[nodes_list, ], na.rm = TRUE)

  # Utilizing the plotrix package to generate a radial plot
  plotrix::radial.plot(t(scenarios_results[nodes_list, ]),
    labels = abbreviate(names.arg = nodes_list, minlength = 6),
    rp.type = "p",
    start = pi / 2,
    radial.lim = c(0, max_value), # Set the radial limits to start from 0
    main = "Comparison of scenarios",
    line.col = "blue",
    lwd = 3
  )
}
