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
#'   \code{\link{createAggregationMatrix}}
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
  coefficients <- lm(y~x)$coefficients[-1] # '-1' to exclude the intercept
  weight <- coefficients / sum(coefficients)

  return(weight)
}


#' Create Aggregation Matrix for Node
#'
#' Creates an aggregation matrix for a node using genetic algorithm
#' optimization.
#'
#' @param aNode A Node object.
#' @param expectedWeight Numeric vector of expected weights.
#' @param nbTables Numeric, number of tables (default is 1).
#' @param popSize Numeric, population size for genetic algorithm (default is
#'   50).
#' @param iters Numeric, number of iterations for genetic algorithm (default is
#'   50).
#'
#' @return A list of aggregation matrices.
#'
#' @aliases createAggregationMatrix.Node
#'
#' @export
#'
#' @importFrom genalg rbga
createAggregationMatrix <- function(aNode,
                                    expectedWeight,
                                    nbTables = 1,
                                    popSize = 50,
                                    iters = 50) {

    # we use an algogen to find the matrix
    # The gene is the value of the aggregated node; the optimisation function is
    # the difference between the expected weight and the actual weight at the
    # power 2
    minValue <- rep(1, dim(aNode@Aggregation)[1])
    maxValue <- rep(aNode@RangeScale, dim(aNode@Aggregation)[1])
    nbChildren <- dim(aNode@Aggregation)[2] - 1
    evaluate <- function(string = c()) {
        # Modify the option with the new gene
        y <- as.integer(round(string))
        x <- aNode@Aggregation[ ,1:nbChildren]
        res <- lm(y~x)$coefficients[-1] / sum(lm(y~x)$coefficients[-1])
        return(sum((expectedWeight - res)^2))
    }

    #Do the job
    rbga.results <- genalg::rbga(minValue,
                                 maxValue,
                                 evalFunc = evaluate,
                                 popSize = popSize,
                                 iters = iters,
                                 verbose = F)
    res1 <- unique(t(round(rbga.results$population)),
                   MARGIN = 2)
    out <- list()
    for(i in 1:nbTables) {
        out[[i]] <- cbind(aNode@Aggregation[ ,1:nbChildren],
                          res1[ ,i])
    }

    return(out)
}
