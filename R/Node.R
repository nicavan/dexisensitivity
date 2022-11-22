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
