#### Tree Class ####

#' Tree Class Definition
#'
#' An S4 class to represent a tree structure.
#'
#' A structured representation of a tree, which includes several slots to store
#' attributes, leaves, nodes, path, and additional information related to the
#' tree structure. The class \code{Tree} is primarily used in the creation,
#' manipulation, and display of tree structures.
#'
#' @slot NumberOfAttributes \code{numeric} - Number of attributes in the tree.
#' @slot NumberOfLeaves \code{numeric} - Number of leaves in the tree.
#' @slot Depth \code{numeric} - Maximum depth of the tree.
#' @slot Attributes \code{character} - Names of attributes in the tree.
#' @slot Leaves \code{character} - Names of leaves in the tree.
#' @slot Aggregated \code{character} - Names of aggregated nodes in the tree.
#' @slot IsMultiple \code{logical} - Flag indicating if multiple leaves are
#'   present in the tree.
#' @slot Multiple \code{data.frame} - List of multiple leaves and their count.
#' @slot IsLeafAggregated \code{logical} - Flag indicating if leaf-aggregated
#'   nodes are present in the tree.
#' @slot LeafAggregated \code{character} - Names of leaf-aggregated nodes in the
#'   tree.
#' @slot Paths \code{list} - Paths from the root to the leaves.
#' @slot Nodes \code{list} - Nodes present in the tree.
#' @slot EvaluationOrder \code{numeric} - Evaluation order for LeafAggregated
#'   nodes.
#' @slot RootName \code{character} - Name of the root node.
#'
#' @return An object of class \code{Tree}.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{print.Tree}}: For printing a Tree object.
#'   \item \code{\link{show.Tree}}: For showing a Tree object.
#'   \item \code{\link{describe.Tree}}: For describing a Tree object.
#' }
#'
#' @name Tree-class
#' @exportClass Tree
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


#### print.Tree ####

#' Print Method for Tree Class Objects
#'
#' Custom print method for objects of class \code{Tree}. This method prints out
#' basic information about the tree, such as the root name, number of
#' attributes, number of leaves, depth, and details about specific nodes.
#'
#' @param x An object of class \code{Tree} that you want to print.
#' @param ... Additional arguments to be passed to the underlying print
#'   function, though they might not have any effect in this custom print
#'   method.
#'
#' @return This function is invoked for its side effect of printing. It does not
#'   return anything.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{Tree-class}}: For the Tree class definition.
#'   \item \code{\link{show.Tree}}: For showing a Tree object.
#'   \item \code{\link{describe.Tree}}: For describing a Tree object.
#' }
#'
#' @aliases print.Tree
#' @aliases \S4method{print}{Tree}
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

    # Repeated aggregated nodes
    repeated_nodes <- names(which(table(x@Aggregated) > 1))
    if (length(repeated_nodes) > 0) {
      cat("\nList of repeated aggregated nodes:\n", repeated_nodes)
    } else {
      cat("\nList of repeated aggregated nodes: Non")
    }

    # Multiple leaves
    if (length(x@IsMultiple) > 0 && x@IsMultiple) {
      cat("\nMultiple leaves:\n")
      print(x@Multiple)
    } else {
      cat("\nNo multiple leaves")
    }

    # Leaf-Aggregated attributes
    if (length(x@IsLeafAggregated) > 0 && x@IsLeafAggregated) {
      cat("\nLeaf-Aggregated attributes:\n")
      print(x@LeafAggregated)
    } else {
      cat("\nNo Leaf-Aggregated Leaf")
    }
  }
)


#### show.Tree ####

#' Show Method for Tree Class Objects
#'
#' Custom show method for objects of class \code{Tree}. It presents a structured
#' representation of the tree using a specific format.
#'
#' Each node of the tree is displayed with its depth, name, and associated twin
#' attributes. Node presentation differs based on its type and position within
#' the tree structure:
#' \itemize{
#'   \item Prefix "Z : " is used for the first node of the tree.
#'   \item Prefix "X : " denotes a leaf node.
#'   \item Prefix "Y : " indicates a non-leaf node.
#' }
#' If a tree has no attributes, it displays "*** Tree without attributes ***".
#'
#' @param object An object of class \code{Tree} to be shown.
#'
#' @return This function is invoked for its side effect of showing a structured
#'   display of the tree. It does not return anything explicitly.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{print.Tree}}: For printing a Tree object.
#'   \item \code{\link{Tree-class}}: For the Tree class definition.
#' }
#'
#' @aliases show.Tree
#' @aliases \S4method{show}{Tree}
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
      digit <- count_digits(object@NumberOfAttributes)

      # Iterate over each attribute of the tree
      for (i in 1:object@NumberOfAttributes) {
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


#' Determine Number of Digits in a Non-negative Number
#'
#' Computes the digit count for a provided non-negative number, returning 1 if
#' the value is 0. Used as an auxiliary function within the package, especially
#' for the `show` method of the `Tree` class to determine the number of digits
#' in `number_of_attributes`.
#'
#' @param number_of_attributes A non-negative number for which the number of
#'   digits is required.
#'
#' @details If the input `number_of_attributes` is 0, the function returns 1.
#' Otherwise, it computes the number of digits using logarithmic operations.
#'
#' @return An integer representing the number of digits in the
#'   `number_of_attributes`.
#'
#' @noRd
count_digits <- function(number_of_attributes) {
  if (number_of_attributes != 0) {
    return(floor(log10(number_of_attributes)) + 1)
  } else {
    return(1)
  }
}


#### describe Method ####

#' Generic Describe Function for Objects
#'
#' Provides a comprehensive description of an object.
#'
#' @param object The object to be described.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{Tree-class}}: For the Tree class definition.
#'   \item \code{\link{print.Tree}}: For printing a Tree object.
#'   \item \code{\link{show.Tree}}: For showing a Tree object.
#' }
#'
#' @export
setGeneric("describe", function(object) {
  standardGeneric("describe")
})


#' Describe Method for Tree Class Objects
#'
#' Outputs a detailed structure of a Tree object, with each node and its
#' properties displayed distinctly.
#'
#' @param object The Tree object to be described.
#'
#' @details Each node within the Tree is presented separately. If the Tree lacks
#' nodes, an error "Tree without any node!" is raised.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{Tree-class}}: For the Tree class definition.
#'   \item \code{\link{print.Tree}}: For printing a Tree object.
#'   \item \code{\link{show.Tree}}: For showing a Tree object.
#' }
#'
#' @return This function is primarily executed for its side effect of presenting
#'   nodes from the Tree object and does not provide a meaningful return value.
#'
#' @aliases describe.Tree
#'
#' @export
setMethod(
  "describe", "Tree",
  function(object) {
    # Nodes are essential for this method
    stopifnot("Tree without any node!" = length(object@Nodes) > 0)

    # Display each node
    object@Nodes |>
      lapply(function(node) {
        methods::selectMethod("print", class(node))(node)
        cat("\n\n")
      }) |>
      invisible() # Ensure only node details are visible
  }
)
