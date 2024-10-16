#### Node Class Definition ####

#'Node Class Definition
#'
#'An S4 class to represent a node in a tree structure.
#'
#'A structured representation of a node, which encompasses various attributes
#'such as name, depth, and relationships with other nodes in the tree structure.
#'The class \code{Node} is primarily used in the creation, manipulation, and
#'display of nodes within tree structures.
#'
#'@slot Id \code{numeric} - Unique sequential identifier for the node.
#'@slot Name \code{character} - Name of the node.
#'@slot IsLeaf \code{logical} - Flag indicating if the node is a leaf.
#'@slot IsLeafAndAggregated \code{logical} - Flag indicating if the node is both
#'  a leaf and an aggregated node.
#'@slot Children \code{character} - List of the node's children names.
#'@slot Sisters \code{character} - List of the node's sisters names.
#'@slot Mother \code{character} - Name of the node's mother.
#'@slot Aggregation \code{matrix} - Aggregation table if the node is aggregated.
#'@slot Probability \code{numeric} - Estimated weight for aggregation.
#'@slot Depth \code{numeric} - Depth of the node in the tree.
#'@slot Twin \code{numeric} - ID of the other leaves for nodes with multiple
#'  leaves.
#'@slot ConditionalProbabilityList \code{list} - List storing conditional
#'  probabilities.
#'@slot RangeScale \code{numeric} - Range scale for the node.
#'@slot ScaleLabel \code{character} - Labels corresponding to different scales.
#'@slot NodePath \code{character} - Path from the root to the leaf for the node.
#'
#'@return An object of class \code{Node}.
#'
#'@seealso
#' \itemize{
#'   \item \code{\link{print.Node}}: For printing a Node object.
#'}
#'
#'@name Node-class
#'@exportClass Node
setClass(
  "Node",
  representation(
    Id = "numeric",
    Name = "character",
    IsLeaf = "logical",
    IsLeafAndAggregated = "logical",
    Children = "character",
    Sisters = "character",
    Mother = "character",
    Aggregation = "matrix",
    Probability = "numeric",
    Depth = "numeric",
    Twin = "numeric",
    ConditionalProbabilityList = "list", # Inutilisé ?
    RangeScale = "numeric",
    ScaleLabel = "character",
    NodePath = "character"
  )
)


#### print Method ####

#' Print Method for Node Class Object
#'
#' Provides a comprehensive display of a Node's properties, such as its name,
#' ID, depth, path, and more. This method is intended for better readability
#' and understanding of a Node's structure and relationships.
#'
#' @param x An object of class \code{Node} that you want to print.
#'
#' @param ... Additional arguments to be passed to the underlying print
#'   function, though they might not have any effect in this custom print
#'   method.
#'
#' @return This function is invoked for its side effect of printing. It does not
#'   return anything.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{Node-class}}: For more details on the Node class.
#'}
#'
#' @aliases print.Node
#' @aliases \S4method{print}{Node}
#'
#' @export
setMethod(
  "print", "Node",
  function(x, ...) {
    cat("Node name:", x@Name)
    cat("\nID:", x@Id)
    cat("\nNode depth:", x@Depth)
    cat("\nFrom root to node: \n ", paste0(x@NodePath, collapse = " -> "))
    cat("\nIs it a leaf:", x@IsLeaf)
    cat(
      "\nIs is a leaf-aggregated:",
      if ((length(x@IsLeafAndAggregated) > 0) && x@IsLeafAndAggregated) {
        "TRUE"
      } else {
        "FALSE"
      }
    )
    cat(
      "\nMother:",
      if (length(x@Mother) == 0) {
        ""
      } else if (is.na(x@Mother)) {
        "Root"
      } else {
        x@Mother
      }
    )
    cat(
      "\nSisters:",
      if (length(x@Sisters) > 0) {
        x@Sisters
      } else {
        "None"
      }
    )
    cat(
      "\nChildren:",
      if (length(x@Children) > 0) {
        x@Children
      } else {
        "None"
      }
    )
    cat("\nEstimated weights:", x@Probability)
  }
)


#### Node Utility Functions ####

#'Estimated Weights for Node
#'
#'Computes weights for each leaf of a given \code{Node} object, using a linear
#'regression model. In this model, the root node's aggregation table values
#'serve as the response variable, and the leaf values as the predictors.
#'
#'Once derived, coefficients (excluding the intercept) represent the relative
#'contributions of each leaf towards the root node. By normalizing these
#'coefficients such that their sum is 1, we obtain an estimate for each leaf's
#'weight.
#'
#'@param node A \code{Node} object.
#'
#'@seealso
#' \itemize{
#'   \item \code{\link{Node-class}}: For more details on the Node class.
#'}
#'
#'@return A numeric vector representing estimated weights for each leaf.
#'
#'@noRd
compute_leaf_weights <- function(node) {
  # Ensure the input is a Node object
  stopifnot("Input should be a Node object" = inherits(node, "Node"))

  # Structure data
  aggregation_table <- node@Aggregation
  y <- aggregation_table[, ncol(aggregation_table)]
  x <- aggregation_table[, -ncol(aggregation_table)]

  # Calculate estimated weights:
  coefficients <- stats::lm(y ~ x)$coefficients[-1] # '-1' to exclude intercept
  weight <- coefficients / sum(coefficients)

  return(weight)
}


#'Aggregation Matrix Creation via Genetic Algorithm
#'
#'Optimizes an aggregation matrix for a specified node.
#'
#'Leverages a genetic algorithm where each "gene" mirrors the value of the
#'aggregated node. This optimization seeks to minimize the squared discrepancy
#'between expected and actual weights, ensuring they align closely with
#'anticipated values.
#'
#'@param node An object of class \code{Node}. Represents the target node.
#'@param expected_weights \code{numeric} vector, denoting the expected weights.
#'@param number_of_tables \code{numeric}. Defines the number of tables; default
#'  is set to 1.
#'@param population_size \code{numeric}. Sets the population size for the
#'  genetic algorithm; default is 50.
#'@param iterations \code{numeric}. Specifies the number of iterations for the
#'  genetic algorithm; default is 50.
#'
#'@return List of aggregation matrices.
#'
#'@seealso
#' \itemize{
#'   \item \code{\link{Node-class}}: For a detailed understanding of the Node class.
#'   \item \code{\link{rbga}}: The genetic algorithm function from the genalg package.
#'   \item \code{\link{print.Node}}: To print a Node object.
#' }
#'
#'@importFrom genalg rbga
#'@noRd
create_aggregation_matrix <- function(node,
                                      expected_weights,
                                      number_of_tables = 1,
                                      population_size = 50,
                                      iterations = 50) {
  # Define the minimum and maximum values for the genetic algorithm
  minValue <- rep(1, nrow(node@Aggregation))
  maxValue <- rep(node@RangeScale, nrow(node@Aggregation))

  nbChildren <- ncol(node@Aggregation) - 1

  # Define the evaluation function for the genetic algorithm
  evaluate <- function(string = c()) {
    y <- as.integer(round(string))
    x <- node@Aggregation[, 1:nbChildren]
    coefficients <- stats::lm(y ~ x)$coefficients[-1]
    weights <- coefficients / sum(coefficients)
    return(sum((expected_weights - weights)^2))
  }

  # Run the genetic algorithm
  rbga_results <- genalg::rbga(minValue, maxValue,
    evalFunc = evaluate,
    popSize = population_size,
    iters = iterations,
    verbose = F
  )

  # Get unique rounded population values
  population_values <- rbga_results$population |>
    round() |>
    t() |>
    unique(MARGIN = 2)

  # Prepare the output list
  out <- list()
  for (i in 1:number_of_tables) {
    out[[i]] <- cbind(node@Aggregation[, 1:nbChildren], population_values[, i])
  }

  return(out)
}
