#' Generate random options matrix for a given rree
#'
#' Create random options based on the `RangeScale` and `Probability` attributes
#' of tree nodes. This is useful for creating random scenarios for simulation or
#' analysis.
#'
#' @param tree Tree object.
#' @param num_options integer. Number of options to generate. Default is 1.
#' @param seed integer. Seed for random number generation. Default is NULL (no
#'   seed set).
#'
#' @return A matrix where rows represent tree leaves and columns represent
#'   sampled options.
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
  if (!is.null(seed)) { set.seed(seed) }

  # Initialize the options matrix
  options_matrix <- matrix(nrow = tree@NumberOfLeaves, ncol = num_options)
  rownames(options_matrix) <- tree@Leaves

  # Populate the matrix with random samples based on node attributes
  for(leaf in tree@Leaves) {
    node_id <- get_id(tree@Nodes, leaf)[1]
    node <- tree@Nodes[[node_id]]

    options_matrix[leaf, ] <- sample(node@RangeScale,
                                    size = num_options,
                                    prob = node@Probability,
                                    replace = TRUE)
  }

  return(options_matrix)
}


#' Evaluate node values in a tree
#'
#' Calculate the values of the tree's nodes by progressing from the leaves
#' (values provided by the option) up to the root. This bottom-up approach
#' ensures that each node's value is determined considering the values of its
#' child nodes.
#'
#' @param tree A Tree object.
#' @param option Matrix representation of a scenario, providing values for the
#'   tree's leaves.
#'
#' @return Numeric vector representing the evaluated values for all nodes, from
#'   leaves to root.
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


#' Assign values to the leaves of the tree
#'
#' Ensure that there are no Leaf-Aggregated leaves in the given option.
#'
#' @param results Numeric vector placeholder for results.
#' @param option A matrix representing a single option/scenario to evaluate.
#'
#' @return Numeric vector with leaf-populated values.
assign_values_to_leaves <- function(results, option) {
  # Assign values from the option matrix to the corresponding leaf in results
  for(i in 1:length(option)) {
    leaf_name <- dimnames(option)[[1]][i]
    results[which(names(results) == leaf_name)] <- option[i]
  }
  return(results)
}


#' Calculate values for leaf-aggregated scenarios
#'
#' Considers tree's evaluation order to derive aggregated values.
#'
#' @param tree A Tree object.
#' @param results Numeric vector placeholder for results.
#'
#' @return Numeric vector with aggregated values.
compute_aggregated_values <- function(tree, results) {
  # Use the tree's order for aggregating results
  for(i in 1:length(tree@EvaluationOrder)) {
    sub_tree <- create_sub_tree(tree, tree@Attributes[tree@EvaluationOrder[i]])
    results <- compute_values_from_aggregation_table(sub_tree, results)
  }
  return(results)
}


#' Compute final aggregated values
#'
#' Uses tree structure to finalize aggregation.
#'
#' @param tree Tree object.
#' @param results Numeric vector placeholder for results.
#'
#' @return Numeric vector with final values.
compute_final_aggregated_values <- function(tree, results) {
  results <- compute_values_from_aggregation_table(tree, results)

  # Propagate values through the main tree
  for(k in tree@Attributes) {
    gotten_id <- get_id(tree@Nodes, k)
    results[gotten_id] <- max(results[gotten_id])
  }

  return(results)
}


#' Compute node values from aggregation table
#'
#' Takes a tree and pre-existing results to determine node values. Iteratively
#' adjusts values based on the aggregation table and related children.
#'
#' @param tree A Tree object.
#' @param results Numeric vector with pre-existing values.
#'
#' @return Numeric vector with updated node values.
compute_values_from_aggregation_table <- function(tree, results) {
  for(agg_nodes_rev in rev(tree@Aggregated)) {
    if (results[agg_nodes_rev] < 0) {
      node_ids <- get_id(tree@Nodes, agg_nodes_rev)

      # Address non-leaf nodes
      if (length(node_ids) > 1) {
        node_ids <- node_ids %>%
          sapply(function(x) {
            if (!tree@Nodes[[x]]@IsLeaf) {x}
          }) %>%
          unlist()
      }

      for(node_id in node_ids) {
        num_children <- length(tree@Nodes[[node_id]]@Children)
        aggregation_table <- tree@Nodes[[node_id]]@Aggregation

        # Adjust values based on child nodes and aggregation table
        for(k in 1:num_children) {
          child_value <- results[tree@Nodes[[node_id]]@Children[k]]
          aggregation_table <- aggregation_table[aggregation_table[, k] == child_value, ]
        }

        results[agg_nodes_rev] <- aggregation_table[num_children + 1]
      }
    }
  }

  return(results)
}


#' Evaluate multiple scenarios for a given tree
#'
#' Uses `evaluate_scenario` to evaluate multiple scenarios simultaneously.
#' Each scenario is represented as a column in the `options_matrix`.
#'
#' @param tree Tree object for evaluation.
#' @param options_matrix Matrix where each column represents a scenario.
#'
#' @return List of numeric vectors with evaluation results for each scenario.
#'
#' @export
evaluate_scenarios <- function(tree, options_matrix) {
  # Using sapply to loop over each column in options_matrix.
  # Each column is treated as a scenario and passed to evaluate_scenario.
  1:dim(options_matrix)[2] %>%
    sapply(function(x) {
      evaluate_scenario(tree, as.matrix(options_matrix[, x]))
    })
}


#' Save options table
#'
#' Stores a matrix of options into a file, primarily for archival or subsequent
#' analysis.
#'
#' @param options_table Matrix containing option values.
#' @param file_name Desired file name for saving the options.
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
                     quote = FALSE)
}


#' Load options table from a file
#'
#' Retrieves a matrix of options saved in a file. This matrix can then be used
#' for further analysis or processing.
#'
#' @param file_name File from which to load the options matrix.
#'
#' @return Matrix of the loaded options.
#'
#' @export
load_options <- function(file_name) {
  # Using utils package to read the table from the file.
  # The choice of tab-separated format matches our save_options function
  # to ensure consistent data interchange.
  as.matrix(utils::read.table(file = file_name,
                              header = TRUE,
                              sep = "\t",
                              row.names = 1))
}


#' Save evaluation results of scenarios to a file
#'
#' Stores the results of scenario evaluations into a file for later analysis.
#'
#' @param theScenarios A list of numeric vectors representing the evaluation
#'   results of the scenarios
#' @param file The name of the file to save the scenarios
#'
#' @return NULL
#'
#' @export
saveScenarios <- function(theScenarios,
                          file) {
    utils::write.table(theScenarios,
                       file = file,
                       sep = "\t",
                       row.names = T,
                       col.names = NA)
}



#' Plot a bar chart of a single scenario
#'
#' Visualizes the values assigned to each attribute in a given scenario and
#' marks the maximum possible value for each attribute using a bar chart.
#'
#' @param aScenario the scenario to graph
#' @param aTree The associated "Tree" object
#' @param isLabelY A logical value indicating whether to include labels on the Y
#'   axis (Default: TRUE)
#' @param isPar A logical value indicating whether to modify the graph's
#'   parameters (Default: TRUE)
#'
#' @return NULL
#'
#' @seealso \code{\link{evaluate_scenario}}
#'
#' @importFrom withr defer
#'
#' @export
showScenario <- function(aScenario,
                         aTree,
                         isLabelY = TRUE,
                         isPar = T) {
    if (isPar) {
        oldpar <- par(mgp = c(7,1,0), oma = c(0,20,0,0), cex = 0.5)
        withr::defer(par(oldpar))
    }

    # Determine the gray scale, we use grey.scale
    myGreyValue <- lapply(1:7, function(x) {grDevices::gray.colors(x, 0, 1) })
    myCol <- aTree@Leaves %>%
        sapply(function(x) {
            myGreyValue[[aTree@Nodes[[get_id(aTree@Nodes, x)[1]]]@RangeScale]][aScenario[x, ]]
        }) %>%
        unlist()

    theMax <- aTree@Attributes %>%
        lapply(function(x) {
            aTree@Nodes[[get_id(aTree@Nodes, x)[1]]]@RangeScale
        }) %>%
        unlist() %>%
        matrix(ncol = 1)

    mc <- graphics::barplot(as.vector(rev(aScenario)),
                            xlim = c(0, max(theMax[]) + 0.5),
                            ylab = "Indicators",
                            xlab = "Mark",
                            horiz = T,
                            col = rev(myCol))

    if (isLabelY) {
        graphics::axis(side = 2,
                       at = mc,
                       labels = rev(rownames(aScenario)),
                       las = 2,
                       cex = 0.5)
    }

    graphics::points(as.vector(rev(theMax)),
                     mc,
                     col = "black",
                     pch = "<")

    graphics::abline(v = c(1:max(theMax)), untf = FALSE, lty = 3)
}


#' Compare scenarios using a radial plot
#'
#' Visualizes the comparison of values assigned to a list of nodes across
#' multiple scenarios using a radial plot.
#'
#' @param aTree A "Tree" object
#' @param theScenarios A list of numeric vectors representing the evaluation
#'   results of the scenarios
#' @param listNodes A list of node names to include in the comparison
#'
#' @seealso \code{\link{evaluate_scenarios}}
#'
#' @return NULL
#' @export
#'
#' @importFrom plotrix radial.plot
compareScenario <- function(aTree,
                            theScenarios,
                            listNodes) {
    oldpar <- par(ps = 6)
    withr::defer(par(oldpar))

    plotrix::radial.plot(t(theScenarios[listNodes, ]),
                         labels = abbreviate(names.arg = listNodes,
                                             minlength = 6),
                         rp.type = "p",
                         start = pi/2,
                         main = "Comparison of scenarios",
                         line.col = "blue",
                         lwd = 3)
}
