# S4 Node Class #
# # # # # # # # #


#### - Node Class Definition - #### ####

#' An S4 class to represent a Node
#'
#' It includes several slots to store information related to the node, such as
#' its name, its depth, and its relationships with other nodes in the tree
#' structure.
#'
#'
#' @slot Id Object of class "numeric", unique sequential id of the node.
#' @slot Name Object of class "character", name of the node.
#' @slot IsLeaf Object of class "logical", indicating if it is a leaf.
#' @slot IsLeafAndAggregated Object of class "logical", indicating if this leaf
#'   is also an aggregated node.
#' @slot Children Object of class "character", list of the names of the node's
#'   children.
#' @slot Sisters Object of class "character", list of the names of the node's
#'   sisters.
#' @slot Mother Object of class "character", name of the node's mother.
#' @slot Aggregation Object of class "matrix", aggregation table if the node is
#'   aggregated.
#' @slot Probability Object of class "numeric", estimated weight of aggregation.
#' @slot Depth Object of class "numeric", depth of the node.
#' @slot Twin Object of class "numeric", id of the other leaves in case of
#'   multiple leaves.
#' @slot ConditionalProbabilityList Object of class "list", list to store
#'   conditional probabilities.
#' @slot RangeScale Object of class "numeric", range scale.
#' @slot ScaleLabel Object of class "character", labels of the different scales.
#' @slot NodePath Object of class "character", node path from root to leaf.
#'
#' @return An object of class Node.
#'
#' @seealso \code{\link{print.Node}}, \code{\link{get_estimated_weights}},
#'   \code{\link{create_aggregation_matrix}}
#'
#' @aliases Node
#'
#' @export
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


#### - print Method - #### ####

#' print method for Node class object
#'
#' Prints basic information about the node including its name, id, depth, path,
#' and other properties.
#'
#' @param x The Node object to be printed.
#'
#' @param ... additional parameters to be passed to the print function.
#'
#' @return No explicit return. Print the Node object.
#'
#' @aliases print.Node
#'
#' @export
setMethod(
  "print","Node",
  function(x, ...) {
    cat("Node name:", x@Name)
    cat("\nID:", x@Id)
    cat("\nNode depth:", x@Depth)
    cat("\nFrom root to node: \n ", paste0(x@NodePath, collapse = " -> "))
    cat("\nIs it a leaf:", x@IsLeaf)
    cat("\nIs is a leaf-aggregated:",
        if ((length(x@IsLeafAndAggregated) > 0) && x@IsLeafAndAggregated) {
          "TRUE"
        } else {"FALSE"})
    cat("\nMother:",
        if (length(x@Mother) == 0) {
          ""
        } else if (is.na(x@Mother)) {
          "Root"
        } else {x@Mother})
    cat("\nSisters:",
        if (length(x@Sisters) > 0) {
          x@Sisters
        } else {"None"})
    cat("\nChildren:",
        if (length(x@Children) > 0) {
          x@Children
        } else {"None"})
    cat("\nEstimated weights:", x@Probability)
  }
)


#### - Utility Functions - #### ####

#' Get Estimated Weights for Node
#'
#' `get_estimated_weights` takes a `Node` object and calculates weights for each
#' of its leaves.
#'
#' The process utilizes a linear regression model, where the root node's
#' aggregation table values form the response variable (denoted as 'y'), and the
#' leaf values are the predictors ('x').
#'
#' Coefficients derived from the model, excluding the intercept, signify the
#' relative contributions of each leaf to the root node. Normalizing these
#' coefficients to sum up to 1 provides an estimate of the weight for each leaf.
#'
#' @param node A Node object.
#'
#' @return A numeric vector of estimated weights.
#'
#' @export
get_estimated_weights <- function(node) {
  # Ensure the input is a Node object
  stopifnot("Input should be a Node object" = inherits(node, "Node"))

  ## Structure data
  aggregation_table <- node@Aggregation
  y <- aggregation_table[, ncol(aggregation_table)]
  x <- aggregation_table[, -ncol(aggregation_table)]

  ## Calcul estimated weights:
  coefficients <- stats::lm(y~x)$coefficients[-1] # '-1' to exclude intercept
  weight <- coefficients / sum(coefficients)

  return(weight)
}


#' Create Aggregation Matrix for Node
#'
#' Creates an aggregation matrix for a node using genetic algorithm
#' optimization.
#'
#' Utilize a genetic algorithm to determine the optimal aggregation matrix.
#' Here, the "gene" corresponds to the value of the aggregated node. The
#' optimization function aims to minimize the squared difference between the
#' expected weight and the actual weight, which ensures that the weights closely
#' match their expected values.
#'
#' @param node A Node object.
#' @param expected_weights Numeric vector of expected weights.
#' @param number_of_tables Numeric, number of tables (default is 1).
#' @param population_size Numeric, population size for genetic algorithm
#'   (default is 50).
#' @param iterations Numeric, number of iterations for genetic algorithm
#'   (default is 50).
#'
#' @return A list of aggregation matrices.
#'
#' @export
#'
#' @importFrom genalg rbga
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
    coefficients <- stats::lm(y~x)$coefficients[-1]
    weights <- coefficients / sum(coefficients)
    return(sum((expected_weights - weights)^2))
  }

  # Run the genetic algorithm
  rbga_results <- genalg::rbga(minValue, maxValue,
                               evalFunc = evaluate,
                               popSize = population_size,
                               iters = iterations,
                               verbose = F)

  # Get unique rounded population values
  population_values <- rbga_results$population %>%
    round() %>%
    t() %>%
    unique(MARGIN = 2)

  # Prepare the output list
  out <- list()
  for(i in 1:number_of_tables) {
    out[[i]] <- cbind(node@Aggregation[, 1:nbChildren], population_values[, i])
  }

  return(out)
}
