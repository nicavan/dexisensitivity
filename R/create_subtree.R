#' Create a Subtree Starting from a Specific Node
#'
#' Constructs a subtree from a specified \code{Node} within a given \code{Tree}.
#' This subtree encompasses all descendant attributes of the \code{Node}.
#'
#' @param tree a \code{Tree} object from which the subtree is derived.
#' @param node_name A \code{character} indicating the name of the starting
#'   \code{Node} for the subtree.
#' @param avoid_repetition A \code{logical} indicating whether to avoid node
#'   repetitions. Default is \code{FALSE}.
#'
#' @return A new \code{Tree} object representing the subtree.
#'
#' @seealso Relevant functions and classes that provide more context or might be
#' of interest:
#' \itemize{
#'   \item \code{\link{Tree-class}}: For an in-depth understanding of the
#'     \code{Tree} class.
#'   \item \code{\link{Node-class}}: To get more details about the structure of
#'     a \code{Node}.
#' }
#'
#' @export
create_sub_tree <- function(tree, node_name, avoid_repetition = FALSE) {
  # Retrieve the node ID corresponding to the provided node name
  selected_node_id <- get_id(tree@Nodes, node_name)

  # If the tree has a leaf-aggregated structure, adjust the selected node ID
  if (tree@IsLeafAggregated) {
    selected_node_id <- adjust_for_leaf_aggregated(tree, selected_node_id)
  }

  # If multiple node IDs are found and repetition should be avoided, use only
  # the first node ID
  if (length(selected_node_id) > 1 & avoid_repetition) {
    selected_node_id <- selected_node_id[1]
  }

  # Stop if the node is a leaf
  if (tree@Nodes[[selected_node_id]]@IsLeaf) {
    stop("Cannot create a subtree from a leaf node")
  }

  # Fetch descendant node IDs
  attribute_ids <- obtain_attribute_ids(tree, selected_node_id)

  # Process node paths
  paths <- lapply(attribute_ids, function(x) {
    node_path <- tree@Nodes[[x]]@NodePath
    depth_start <- tree@Nodes[[selected_node_id]]@Depth
    path_length <- length(node_path)
    return(node_path[depth_start:path_length])
  })


  # Refactored code to modify tree nodes
  tree_nodes <- modify_tree_nodes(tree, attribute_ids, paths)

  # Extract names from modified tree nodes
  leaf_names <- extract_names(tree_nodes, is_leaf = TRUE)
  aggregated_names <- extract_names(tree_nodes, is_leaf = FALSE)

  # Handle multiple nodes and LeafAggregated nodes
  multiple <- handle_multiple(tree, tree_nodes, leaf_names)
  is_multiple <- is.matrix(multiple)
  # Note dev :
  #  If handle_multiple don't detect multiple, it returns a data.frame

  leaf_aggregated <- handle_leaf_aggregated(
    tree, tree_nodes,
    leaf_names, aggregated_names
  )
  is_leaf_aggregated <- length(leaf_aggregated) > 0 & all(leaf_aggregated != "")

  # Update Twin and IsLeafAndAggregated attributes
  for (i in seq_along(leaf_aggregated)) {
    dup <- get_id(tree_nodes, leaf_aggregated[i])
    for (j in seq_along(dup)) {
      tree_nodes[[dup[j]]]@Twin <- setdiff(dup, tree_nodes[[dup[j]]]@Id)
      tree_nodes[[dup[j]]]@IsLeafAndAggregated <- TRUE
    }
  }


  # Remove leaf-aggregated from leaves if it exists
  if (is_leaf_aggregated) {
    leaf_names <- setdiff(leaf_names, leaf_aggregated)
  }

  nb_leaves <- length(leaf_names)
  nb_levels <- max(sapply(paths, length))

  attribute_ids <- attribute_ids |>
    sapply(function(x) {
      tree@Nodes[[x]]@Name
    })

  sub_tree <- new("Tree",
    RootName = node_name,
    NumberOfAttributes = length(tree_nodes),
    NumberOfLeaves = nb_leaves,
    Depth = nb_levels,
    Nodes = tree_nodes,
    Multiple = as.data.frame(multiple),
    IsMultiple = is_multiple,
    IsLeafAggregated = is_leaf_aggregated,
    LeafAggregated = leaf_aggregated,
    Attributes = attribute_ids,
    Leaves = leaf_names,
    Aggregated = aggregated_names,
    EvaluationOrder = numeric(0),
    Paths = paths
  )

  sub_tree@EvaluationOrder <- evaluate_order(sub_tree)

  return(sub_tree)
}


#' Adjust Node ID for Trees with Leaf-Aggregated Structures
#'
#' When a \code{Tree} has a leaf-aggregated structure, the node ID may require
#' adjustment. This function carries out the necessary transformation for such
#' nodes.
#'
#' @param tree a \code{Tree} object that might possess a leaf-aggregated
#'   structure.
#' @param node_id a \code{numeric} or \code{character} representing the
#'   identifier for the \code{Node} to be adjusted.
#'
#' @return A \code{numeric} vector encompassing the adjusted node IDs.
#'
#' @keywords internal
adjust_for_leaf_aggregated <- function(tree, node_id) {
  adjusted_id <- node_id |>
    sapply(function(x) {
      if (!tree@Nodes[[x]]@IsLeaf) {
        tree@Nodes[[x]]@Id
      }
    }) |>
    unlist()
  return(adjusted_id)
}


#' Identify Descendant Attribute IDs of a Node
#'
#' Retrieves all nodes that share a path with the specified node. Such nodes are
#' considered descendants of the given node and are included in the subtree.
#'
#' @param tree a \code{Tree} object.
#' @param node_id a \code{numeric} or \code{character} identifier representing
#'   the \code{Node} that will act as the reference point.
#'
#' @return A \code{numeric} vector of attribute IDs symbolizing descendant
#'   nodes.
#'
#' @keywords internal
obtain_attribute_ids <- function(tree, node_id) {
  # Extract attribute IDs based on the node path
  attribute_ids <- tree@Nodes |>
    sapply(function(x) {
      if (grep(paste(tree@Nodes[[node_id]]@NodePath, collapse = " "),
               paste(x@NodePath, collapse = " "),
               fixed = TRUE) |>
          length()) {
        x@Id
      }
    }) |>
    unlist()

  return(attribute_ids)
}


#' Modify Node Attributes for Subtree Construction
#'
#' Adjusts attributes of nodes in the process of constructing the subtree.
#' This ensures correct relationships and hierarchies within the new subtree.
#'
#' @param tree a \code{Tree} object representing the original tree structure.
#' @param attribute_ids a \code{numeric} vector containing the node identifiers
#'   that are being processed.
#' @param paths a list of \code{numeric} vectors, representing the node paths
#'   associated with the nodes being processed.
#'
#' @return A list of \code{Node} objects with modified attributes tailored for
#'   the subtree construction.
#'
#' @keywords internal
modify_tree_nodes <- function(tree, attribute_ids, paths) {
  tree_nodes <- tree@Nodes[attribute_ids]
  for (i in seq_along(attribute_ids)) {
    tree_nodes[[i]]@Id <- i
    tree_nodes[[i]]@NodePath <- paths[[i]]
    tree_nodes[[i]]@Depth <- length(tree_nodes[[i]]@NodePath)
    tree_nodes[[i]]@IsLeafAndAggregated <- FALSE
    tree_nodes[[i]]@Twin <- integer(0)
    if (i == 1) {
      tree_nodes[[i]]@Mother <- as.character(NA)
    }
  }
  return(tree_nodes)
}


#' Extract Node Names from Tree Nodes
#'
#' Retrieves names of nodes based on their leaf status.
#'
#' @param tree_nodes a list of \code{Node} objects from which names are to be
#'   extracted.
#' @param is_leaf a \code{logical} value indicating whether names should be
#'   extracted from leaf nodes (if \code{TRUE}) or from non-leaf nodes (if
#'   \code{FALSE}).
#'
#' @return A \code{character} vector containing node names.
#'
#' @keywords internal
extract_names <- function(tree_nodes, is_leaf = TRUE) {
  tree_nodes |>
    sapply(function(x) {
      if (x@IsLeaf == is_leaf) x@Name
    }) |>
    unlist()
}


#' Process and Handle Multiple Nodes
#'
#' Manages scenarios where nodes might appear multiple times within the tree
#' structure.
#'
#' @param tree a \code{Tree} object representing the original tree.
#' @param tree_nodes a list of \code{Node} objects that are currently under
#'   consideration.
#' @param leaf_names a \code{character} vector containing the names of leaf
#'   nodes.
#'
#' @return A \code{matrix} detailing occurrences of nodes. If no multiple nodes
#'   are found, a \code{data.frame} is returned instead.
#'
#' @keywords internal
handle_multiple <- function(tree, tree_nodes, leaf_names) {
  if (!tree@IsMultiple) {
    return(data.frame(Occ = NA))
  }

  multiple <- table(leaf_names)
  if (max(multiple) <= 1) {
    return(data.frame(Occ = NA))
  }

  multiple <- as.matrix(multiple[multiple > 1])
  colnames(multiple) <- "Occ"

  return(multiple)
}


#' Handle Leaf-Aggregated Nodes in a Tree
#'
#' Manages nodes that are both leaves and aggregated within the tree structure.
#'
#' @param tree a \code{Tree} object representing the original tree.
#' @param tree_nodes a list of \code{Node} objects that are currently under
#'   consideration.
#' @param leaf_names a \code{character} vector containing the names of leaf
#'   nodes.
#' @param aggregated_names a \code{character} vector containing the names of
#'   aggregated nodes.
#'
#' @return A \code{character} vector containing names of nodes that are both
#'   leaves and aggregated. If no such nodes are found, an empty string is
#'   returned.
#'
#' @keywords internal
handle_leaf_aggregated <- function(tree,
                                   tree_nodes,
                                   leaf_names,
                                   aggregated_names) {
  if (!tree@IsLeafAggregated) {
    return(character(0))
  }

  leaf_aggregated <- intersect(leaf_names, aggregated_names)
  if (length(leaf_aggregated) == 0) {
    return(character(0))
  }

  return(leaf_aggregated)
}
