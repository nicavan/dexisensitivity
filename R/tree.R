# S4 Tree Class #
#################

#' Tree
#'
#' Tree class definition
#'
#' @slot nbAttributes (numeric) Number of attributes
#' @slot nbLeaves (numeric) Number of leaves
#' @slot Depth (numeric) Maximum depth of the tree
#' @slot Attributes (character) List of names of attributes
#' @slot Leaves (character) List of names of leaves
#' @slot Aggregated (character) List of names of aggregated nodes
#' @slot isMultiple (logical) Tag to know if multiple leaves
#' @slot Multiple (data.frame) List of the multiple leaves and number of
#'   occurrence
#' @slot isLeafAggregated (logical) Tag to know if leaf-aggregated nodes
#' @slot LeafAggregated (character) List of names of leaf-aggregated nodes
#' @slot Paths (list) Path from root to leafs
#' @slot Nodes (list) List of nodes
#' @slot EvalOrder (numeric) Evaluation order in case of LeafAggregated nodes
#' @slot rootName (character) Name of the root node
#'
#' @return
#' @export
#' @import methods
#' @examples
methods::setClass("Tree",
         representation(nbAttributes     = "numeric",
                        nbLeaves         = "numeric",
                        Depth            = "numeric",
                        Attributes       = "character",
                        Leaves           = "character",
                        Aggregated       = "character",
                        isMultiple       = "logical",
                        Multiple         = "data.frame",
                        isLeafAggregated = "logical",
                        LeafAggregated   = "character",
                        Paths            = "list",
                        Nodes            = "list",
                        EvalOrder        = "numeric",
                        rootName         = "character"
         ))




#' print.tree
#'
#' Print method for Tree class object
#'
#' @param x The printed Tree
#'
#' @param ... see '...' print parameters
#'
#'
#' @return
#'
#' @examples
setMethod("print", "Tree",
          function(x,...) {
              cat("Root name:", x@rootName)
              cat("\nNumber of attributes:", length(x@Attributes))
              cat("\nNumber of aggregated attributes:", length(x@Aggregated))
              cat("\nNumber of true leaves (no multiple, no aggregated):",
                  x@nbLeaves)
              cat("\nMaximum depth:", x@Depth)
              cat("\nList of repeated aggregated nodes:",
                  if(length(which(table(x@Aggregated) > 1))) {
                      names(which(table(x@Aggregated)>1))
                  } else {"Non"}
                  )
              if(length(x@isMultiple) > 0 && x@isMultiple) {
                  cat("\nMultiple leaves: \n")
                  print(x@Multiple)
                  } else {cat("\nNo multiple leaves")}
              if(length(x@isLeafAggregated) > 0 && x@isLeafAggregated) {
                  cat("\nLeaf-Aggregated attributes: \n")
                  print(x@LeafAggregated)
                  } else {cat("\nNo Leaf-Aggregated Leaf")}
          }
)
