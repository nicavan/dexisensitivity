# S4 Tree Class #
#################

#' An S4 class to represent a Tree
#'
#' It includes several slots to store attributes, leaves, nodes, path, and
#' additional information related to the tree structure.
#'
#' @slot nbAttributes Object of class "numeric", the number of attributes in the
#'   tree.
#' @slot nbLeaves Object of class "numeric", the number of leaves in the tree.
#' @slot Depth Object of class "numeric", the maximum depth of the tree.
#' @slot Attributes Object of class "character", list of names of attributes in
#'   the tree.
#' @slot Leaves Object of class "character", list of names of leaves in the
#'   tree.
#' @slot Aggregated Object of class "character", list of names of aggregated
#'   nodes in the tree.
#' @slot isMultiple Object of class "logical", flag indicating if there are
#'   multiple leaves in the tree.
#' @slot Multiple Object of class "data.frame", list of multiple leaves and
#'   their count.
#' @slot isLeafAggregated Object of class "logical", flag indicating if there
#'   are leaf-aggregated nodes in the tree.
#' @slot LeafAggregated Object of class "character", list of names of
#'   leaf-aggregated nodes in the tree.
#' @slot Paths Object of class "list", list of paths from the root to the
#'   leaves.
#' @slot Nodes Object of class "list", list of nodes in the tree.
#' @slot EvalOrder Object of class "numeric", evaluation order in case of
#'   LeafAggregated nodes.
#' @slot rootName Object of class "character", name of the root node.
#'
#' @method Tree class objects are used in the creation and display of tree
#' structures. Information from the object is used to create and print the tree
#' structure in a human-readable format.
#'
#' @return An object of class Tree.
#'
#' @seealso \code{\link{print.Tree}}, \code{\link{show.Tree}},
#' \code{\link{describe.Tree}}
#'
#' @export
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
                  )
)


#' print method for Tree class object
#'
#' Custom print method for Tree class object. Prints basic information about the
#' tree including root name, number of attributes, leaves, depth and specific
#' nodes.
#'
#' @param x Tree object to be printed.
#' @param ... additional parameters to be passed to the print function.
#'
#' @aliases print.Tree
#'
#' @return
#'
#' @export
setMethod("print", "Tree",
          function(x, ...) {
              cat("Root name:", x@rootName)
              cat("\nNumber of attributes:", length(x@Attributes))
              cat("\nNumber of aggregated attributes:", length(x@Aggregated))
              cat("\nNumber of true leaves (no multiple, no aggregated):",
                  x@nbLeaves)
              cat("\nMaximum depth:", x@Depth)
              cat("\nList of repeated aggregated nodes:",
                  if(length(which(table(x@Aggregated) > 1))) {
                      names(which(table(x@Aggregated) > 1))
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



#' show method for Tree class object
#'
#' Custom show method for Tree class object. Prints a formatted structure of the
#' tree using a set of rules based on node properties.
#'
#' @param object The Tree object to be shown.
#'
#' @return
#'
#' @aliases show.Tree
#'
#' @export
setMethod("show", "Tree",
          function(object) {

              if(identical(object@nbAttributes, numeric(0))) {
                  cat("*** Tree without attributes ***")
              } else {

                  if(object@nbAttributes != 0) {
                      digit <- floor(log10(object@nbAttributes)) + 1
                  } else {
                      digit <- 1
                  }

                  for(i in 1:object@nbAttributes) {

                      if (i == 1) {
                          prefix <- "Z : "
                      } else if (object@Nodes[[i]]@isLeaf) {
                          prefix <- "X : "
                      } else {
                          prefix <- "Y : "
                      }

                      cat("< ", formatC(i, width = digit), " > ",
                          rep("- ", (object@Nodes[[i]]@Depth - 1)),
                          prefix, object@Nodes[[i]]@name,
                          if(length(object@Nodes[[i]]@Twin)) {
                              paste0(" [", c(object@Nodes[[i]]@Twin), "]")
                          }, "\n", sep = "")
                  }
              }
          }
)

#' Title
#'
#' @param object the Tree
#'
#' @return
#'
#' @export
setGeneric("describe", function(object) {standardGeneric("describe")})

#' describe method for Tree class object
#'
#' A method to describe the structure of a Tree object in a comprehensive way.
#' It prints each node and its properties separately.
#'
#' @param Tree The Tree object to be described.
#'
#' @param object Object
#'
#' @aliases describe.Tree
#'
#' @return
#'
#' @export
setMethod("describe", "Tree", function(object) {
    stopifnot("Tree without any node!" = length(object@Nodes)>0)

    object@Nodes %>%
        lapply(function(y) {
            selectMethod("print", class(y))(y)
            cat("\n\n")}
        ) %>%
        invisible()
}
)

