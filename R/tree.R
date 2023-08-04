# S4 Tree Class #
# # # # # # # # #


#### - Tree Class Definition - #### ####
#' An S4 class to represent a Tree
#'
#' It includes several slots to store attributes, leaves, nodes, path, and
#' additional information related to the tree structure.
#'
#' Tree class objects are used in the creation and display of tree structures.
#' Information from the object is used to create and print the tree structure in
#' a human-readable format.
#'
#' @slot NumberOfAttributes Object of class "numeric", the number of attributes
#'   in the tree.
#' @slot NumberOfLeaves Object of class "numeric", the number of leaves in the
#'   tree.
#' @slot Depth Object of class "numeric", the maximum depth of the tree.
#' @slot Attributes Object of class "character", list of names of attributes in
#'   the tree.
#' @slot Leaves Object of class "character", list of names of leaves in the
#'   tree.
#' @slot Aggregated Object of class "character", list of names of aggregated
#'   nodes in the tree.
#' @slot IsMultiple Object of class "logical", flag indicating if there are
#'   multiple leaves in the tree.
#' @slot Multiple Object of class "data.frame", list of multiple leaves and
#'   their count.
#' @slot IsLeafAggregated Object of class "logical", flag indicating if there
#'   are leaf-aggregated nodes in the tree.
#' @slot LeafAggregated Object of class "character", list of names of
#'   leaf-aggregated nodes in the tree.
#' @slot Paths Object of class "list", list of paths from the root to the
#'   leaves.
#' @slot Nodes Object of class "list", list of nodes in the tree.
#' @slot EvaluationOrder Object of class "numeric", evaluation order in case of
#'   LeafAggregated nodes.
#' @slot RootName Object of class "character", name of the root node.
#'
#' @return An object of class Tree.
#'
#' @seealso \code{\link{print.Tree}}, \code{\link{show.Tree}},
#'   \code{\link{describe.Tree}}
#'
#' @aliases Tree
#'
#' @export
methods::setClass(
  "Tree",
  representation(
    NumberOfAttributes = "numeric",
    NumberOfLeaves     = "numeric",
    Depth              = "numeric",
    Attributes         = "character",
    Leaves             = "character",
    Aggregated         = "character",
    IsMultiple         = "logical",
    Multiple           = "data.frame",
    IsLeafAggregated   = "logical",
    LeafAggregated     = "character",
    Paths              = "list",
    Nodes              = "list",
    EvaluationOrder    = "numeric",
    RootName           = "character"
  )
)


#### - print Method - #### ####
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
#' @return No explicit return. Print the Tree object.
#'
#' @export
setMethod(
  "print", "Tree",
  function(x, ...) {
    cat("Root name:", x@RootName)
    cat("\nNumber of attributes:", length(x@Attributes))
    cat("\nNumber of aggregated attributes:", length(x@Aggregated))
    cat("\nNumber of true leaves (no multiple, no aggregated):",
        x@NumberOfLeaves)
    cat("\nMaximum depth:", x@Depth)
    cat("\nList of repeated aggregated nodes:",
        if(length(which(table(x@Aggregated) > 1))) {
          names(which(table(x@Aggregated) > 1))
        } else {"Non"}
    )

    if(length(x@IsMultiple) > 0 && x@IsMultiple) {
      cat("\nMultiple leaves: \n")
      print(x@Multiple)
    } else {cat("\nNo multiple leaves")}

    if(length(x@IsLeafAggregated) > 0 && x@IsLeafAggregated) {
      cat("\nLeaf-Aggregated attributes: \n")
      print(x@LeafAggregated)
    } else {cat("\nNo Leaf-Aggregated Leaf")}
  }
)


#### - show Method - #### ####
#' show method for Tree class object
#'
#' Custom show method for Tree class object. Prints a formatted structure of the
#' tree using a set of rules based on node properties.
#'
#' @description Each node of the tree is printed with its depth, name, and twin
#'   attributes. Different prefixes are used depending on whether the node is
#'   the first one (prefix "Z : "), a leaf node (prefix "X : "), or a non-leaf
#'   node (prefix "Y : "). If a tree has no attributes, it prints "*** Tree
#'   without attributes ***".
#'
#' @param object The Tree object to be shown.
#'
#' @return No explicit return. Show the Tree class object.
#'
#' @aliases show.Tree
#'
#' @export
setMethod(
  "show", "Tree",
  function(object) {

    # The tree might not have any attributes - handle this edge case first
    if (identical(object@NumberOfAttributes, numeric(0))) {
      cat("*** Tree without attributes ***")
    } else {
      # Digit is used to ensure proper formatting when printing the Tree
      digit <- calculate_digit(object@NumberOfAttributes)

      # Iterate over each attribute of the tree
      for(i in 1:object@NumberOfAttributes) {

        # We use different prefixes to signify the type and position of a node
        if (i == 1) {
          prefix <- "Z : "
        } else if (object@Nodes[[i]]@IsLeaf) {
          prefix <- "X : "
        } else {
          prefix <- "Y : "
        }

        # Not every node has a twin - handle this case to prevent errors
        if (length(object@Nodes[[i]]@Twin)) {
          twin <- paste0(" [", c(object@Nodes[[i]]@Twin), "]")
        } else {
          twin <- ""
        }

        # Print the tree in a specific, structured format for readability
        cat(
          "< ", formatC(i, width = digit), " > ",
          rep("- ", (object@Nodes[[i]]@Depth - 1)),
          prefix, object@Nodes[[i]]@Name,
          twin, "\n",
          sep = ""
        )
      }
    }
  }
)


#' Calculate Digit
#'
#' Calculates the number of digits of a given non-negative number. Returns 1 if
#' the input is 0.
#'
#' @description This is an internal helper function used within the package.
#'   It's used in the `show` method for the `Tree` class to calculate the number
#'   of digits in `number_of_attributes`.
#'
#' @param number_of_attributes A non-negative number representing the number of
#'   attributes.
#'
#' @return The number of digits in the `number_of_attributes`. Returns 1 if
#'   `number_of_attributes` is 0.
#'
#' @keywords internal
#'
calculate_digit <- function(number_of_attributes) {
  if (number_of_attributes != 0) {
    return(floor(log10(number_of_attributes)) + 1)
  } else {
    return(1)
  }
}


#### - describe Method - #### ####
#' Title
#'
#' @param object the Tree
#'
#' @return No explicit return.
#'
#' @export
setGeneric("describe", function(object) {standardGeneric("describe")})


#' describe method for Tree class object
#'
#' A method to describe the structure of a Tree object in a comprehensive way.
#' It prints each node and its properties separately.
#'
#' @param object Object
#'
#' @aliases describe.Tree
#'
#' @return Primarily invoked for its side effect of displaying the nodes of the
#'   Tree object, the function does not produce a meaningful return value.
#'
#' @export
setMethod(
  "describe", "Tree",
  function(object) {
    # Nodes are necessary for this method
    stopifnot("Tree without any node!" = length(object@Nodes) > 0)

    # Call print for every node
    object@Nodes %>%
      lapply(function(y) {
        selectMethod("print", class(y))(y)
        cat("\n\n")
      }) %>%
      invisible() # we only want to see node prints
  }
)

