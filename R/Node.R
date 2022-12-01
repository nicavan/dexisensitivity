# S4 Node Class #
#################

#' Title
#'
#' @slot id (numeric) Unique sequential id of the node.
#' @slot name (character) Name of the node.
#' @slot isLeaf (logical) Is it a leaf?
#' @slot isLeafAndAggregated (logical) Is this leaf also an aggregated node!
#' @slot children (character) List of the names of the node's children.
#' @slot sisters (character) List of the names of the node's sisters.
#' @slot mother (character) Name of the node's mother.
#' @slot aggregation (matrix) If aggregated node, table of aggregation.
#' @slot Proba (numeric) Estimated weight of aggregation. If Leaf set basically to uniform.
#' @slot Depth (numeric) Depth of the node.
#' @slot Twin (numeric) In case of multiple leaves, give the id of the other leaves.
#' @slot CondiProbaList (list) ???????????
#' @slot rangeScale (numeric) Range scale.
#' @slot scaleLabel (character) Labels of the different scales.
#' @slot nodePath (character) Node path from root to leaf.
#'
#' @return
#' @export
#'
#' @examples
setClass("Node",
         representation(id = "numeric",
                        name = "character",
                        isLeaf = "logical",
                        isLeafAndAggregated = "logical",
                        children = "character",
                        sisters = "character",
                        mother = "character",
                        aggregation = "matrix",
                        Proba = "numeric",
                        Depth = "numeric",
                        Twin = "numeric",
                        CondiProbaList = "list",
                        rangeScale = "numeric",
                        scaleLabel = "character",
                        nodePath = "character"
         )
)



#' print.node
#'
#' Print method for Node class object
#'
#' @param x The printed Node
#'
#' @param ... see '...' print prameters
#'
#' @return
#' @export
#'
#' @examples
setMethod("print","Node",
          function(x,...){
              cat("Node name:", x@name)
              cat("\nID:", x@id)
              cat("\nNode depth:", x@Depth)
              cat("\nFrom root to node:", paste0(x@nodePath, col = "->"))
              cat("\nIs it a leaf:", x@isLeaf)
              cat("\nIs is a leaf-aggregated:",
                  if (length(x@isLeafAndAggregated) && x@isLeafAndAggregated) {
                      "TRUE"
                      } else {"FALSE"})
              cat("\nMother:",
                  if (length(x@mother) == 0) {
                      ""
                  } else if (is.na(x@mother)) {
                      "Root"
                  } else {x@mother})
              cat("\nSisters:",
                  if (length(x@sisters) && length(x@sisters)) {
                      x@sisters
                      } else {"None"})
              cat("\nChildren:",
                  if (length(x@children) && length(x@children)) {
                      x@children
                      } else {"None"})
              cat("\nEstimated weights:", x@Proba)
              }
          )
