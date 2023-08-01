# S4 Node Class #
#################

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
#' @slot ConditionalProbabilityList Object of class "list", list to store conditional
#'   probabilities.
#' @slot RangeScale Object of class "numeric", range scale.
#' @slot ScaleLabel Object of class "character", labels of the different scales.
#' @slot NodePath Object of class "character", node path from root to leaf.
#'
#' @return An object of class Node.
#'
#' @seealso \code{\link{print.Node}}, \code{\link{getEstimatedWeights}},
#' \code{\link{createAggregationMatrix}}
#'
#' @aliases Node
#'
#' @export
setClass("Node",
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



#' print method for Node class object
#'
#' Prints basic information about the node including its name, id, depth, path,
#' and other properties.
#'
#' @param x The Node object to be printed.
#'
#' @param ... additional parameters to be passed to the print function.
#'
#' @return
#'
#' @aliases print.Node
#'
#' @export
setMethod("print","Node",
          function(x,...){
              cat("Node name:", x@Name)
              cat("\nID:", x@Id)
              cat("\nNode depth:", x@Depth)
              cat("\nFrom root to node:", paste0(x@NodePath, col = "->"))
              cat("\nIs it a leaf:", x@IsLeaf)
              cat("\nIs is a leaf-aggregated:",
                  if (length(x@IsLeafAndAggregated) && x@IsLeafAndAggregated) {
                      "TRUE"
                  } else {"FALSE"})
              cat("\nMother:",
                  if (length(x@Mother) == 0) {
                      ""
                  } else if (is.na(x@Mother)) {
                      "Root"
                  } else {x@Mother})
              cat("\nSisters:",
                  if (length(x@Sisters) && length(x@Sisters)) {
                      x@Sisters
                  } else {"None"})
              cat("\nChildren:",
                  if (length(x@Children) && length(x@Children)) {
                      x@Children
                  } else {"None"})
              cat("\nEstimated weights:", x@Probability)
          }
)



#' Get Estimated Weights for Node
#'
#' Calculates the estimated weights from the aggregation table of a Node object.
#'
#' @param aNode A Node object.
#'
#' @return A numeric vector of estimated weights.
#'
#' @aliases getEstimatedWeights.Node
#'
#' @export
getEstimatedWeights <- function(aNode) {
    aggregationTable <- aNode@Aggregation
    y <- aggregationTable[ ,dim(aggregationTable)[2]]
    x <- aggregationTable[ ,-c(dim(aggregationTable)[2])]
    res <- (lm(y~x)$coefficients[-1]) / (sum(lm(y~x)$coefficients[-1]))
    return(res)
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
