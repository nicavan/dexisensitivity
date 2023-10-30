#' Tree Construction from DEXi's XML Output
#'
#' Constructs a \code{Tree} objects using the XML representation from a DEXi
#' model.
#'
#' Iterates over every root attribute in the provided XML to create tree
#' structures that depict the hierarchical decision model.
#'
#' @param main_tree An object of class \code{XML}. Represents the primary tree
#'   structure and should encompass the DEXi decision model.
#'
#' @return A list of \code{Tree} objects, each corresponding to a unique root
#'   attribute in the DEXi XML layout. Each tree details the root attribute,
#'   nodes, leaves, depth, and other associated data regarding the decision
#'   model's configuration.
#'
#' @import XML
#' @importFrom AlgDesign gen.factorial
#'
#' @export
create_tree <- function(main_tree) {
  # Get root(s) name(s) of the DEXi Tree
  roots_xml <- XML::getNodeSet(doc = main_tree, path = "/DEXi/ATTRIBUTE/NAME")
  list_root_name <- sapply(roots_xml, XML::xmlValue)

  list_tree <- list() # To stock created Trees

  # For each Tree's roots
  for (i_root_name in seq_along(list_root_name)) {
    root_name <- list_root_name[i_root_name]
    attributes <- get_dexi_attributes(main_tree, root_name)
    number_of_attributes <- length(attributes)
    list_path <- get_paths(attributes, main_tree, root_name)

    # Creates the nodes
    tree_nodes <- lapply(list_path, create_node, main_tree = main_tree)
    for (i in 1:number_of_attributes) {
      tree_nodes[[i]]@Id <- i
    }

    # Maximum tree depth
    number_of_levels <- max(sapply(list_path, length))

    # List of leaves
    leaves <- sapply(
      tree_nodes,
      function(x) {
        if (x@IsLeaf) x@Name
      }
    ) |>
      unlist()

    # List of Aggregated
    aggregated <- sapply(
      tree_nodes,
      function(x) {
        if (!x@IsLeaf) x@Name
      }
    ) |>
      unlist()

    # Leaf-Aggregated attribute
    leaf_aggregated_result <- process_aggregated_leaf(
      tree_nodes,
      leaves,
      aggregated
    )

    # Extract the result
    leaf_aggregated <- leaf_aggregated_result$leaf_aggregated
    is_leaf_aggregated <- leaf_aggregated_result$is_leaf_aggregated
    tree_nodes <- leaf_aggregated_result$tree_nodes

    # Check for duplicated leaves
    duplicated_leaf_result <- process_duplicated_leaf(tree_nodes, leaves)
    multiple <- duplicated_leaf_result$multiple
    is_multiple <- duplicated_leaf_result$is_multiple
    tree_nodes <- duplicated_leaf_result$tree_nodes

    # Get the true leaves
    leaves <- unique(leaves) # Remove duplicates
    if (is_leaf_aggregated) {
      # Find the indices of elements in leaves that are also in leaf_aggregated
      indices_to_remove <- which(leaves %in% leaf_aggregated)

      # Remove those indices from 'leaves'
      leaves <- leaves[-indices_to_remove]
    }
    number_of_leaves <- length(leaves)

    # Define the order to evaluate a tree if LeafAggregated node
    tree <- new("Tree",
      RootName = root_name,
      NumberOfAttributes = number_of_attributes,
      NumberOfLeaves = number_of_leaves,
      Depth = number_of_levels,
      Nodes = tree_nodes,
      Multiple = as.data.frame(multiple),
      IsMultiple = is_multiple,
      IsLeafAggregated = is_leaf_aggregated,
      LeafAggregated = leaf_aggregated,
      Attributes = attributes,
      Leaves = leaves,
      Aggregated = aggregated,
      EvaluationOrder = numeric(0),
      Paths = list_path
    )

    tree@EvaluationOrder <- evaluate_order(tree)

    list_tree[[i_root_name]] <- tree
  }

  return(list_tree)
}


#' Retrieve Attribute Names from DEXi XML Tree
#'
#' Extracts all attribute names under a specified root node from a DEXi XML
#' tree. Constructs a search path within the XML structure to capture all
#' descendant attributes beginning from the provided root name. This function is
#' primarily an internal utility employed by \code{\link{create_tree}} to help
#' process the XML tree structure.
#'
#' @param main_tree \code{XML} object representing the main tree of the DEXi
#'   model. It should be a valid XML containing the DEXi decision model
#'   structure.
#' @param root_name \code{character} indicating the name of the root attribute
#'   for initiating the search. This name should be present as an attribute
#'   within the provided XML structure.
#'
#' @return \code{character} vector of attribute names. Begins with the specified
#'   root attribute's name and continues with its descendant attributes in their
#'   order from the XML tree.
#'
#' @seealso For functions that further process or utilize the DEXi XML
#' structure:
#' \itemize{
#'   \item \code{\link{create_tree}}: The main function that utilizes this
#'     internal function to construct a tree from the DEXi XML output.
#' }
#'
#' @keywords internal
get_dexi_attributes <- function(main_tree, root_name) {
  to_search <- paste0("//ATTRIBUTE[NAME='", root_name, "']//ATTRIBUTE/NAME")
  attributes <- c(
    root_name,
    XML::xmlValue(XML::getNodeSet(main_tree, to_search))
  )
}


#' Retrieve Attribute Paths in an XML Tree
#'
#' Navigates through an XML tree to identify specific paths corresponding to a
#' set of attributes, beginning from a specified root. This function is an
#' internal tool, chiefly called by \code{\link{create_tree}} to aid in
#' interpreting and structuring the XML tree.
#'
#' @param attributes \code{character} vector of attribute names whose paths are
#'   to be located.
#' @param main_tree \code{XML} object representing the tree in which paths are
#'   to be searched.
#' @param root_name \code{character} indicating the name of the root attribute
#'   from where the search begins.
#'
#' @return A list of paths where each path is a \code{character} vector,
#'   detailing the sequence of attributes from the root to the desired
#'   attribute.
#'
#' @seealso Primary functions that integrate or work with the XML structure:
#' \itemize{
#'   \item \code{\link{create_tree}}: The main function that invokes
#'     this internal function to dissect the XML output of DEXi.
#' }
#'
#' @keywords internal
get_paths <- function(attributes, main_tree, root_name) {
  list_path <- list()
  list_path[[1]] <- root_name

  # Loop through attributes to find paths.
  # Start from second attribute as first is the root (root_name).
  for (i in seq_along(attributes)[-1]) {
    path_found <- FALSE

    # Iterate over the potential path nodes in reverse.
    # Reversing the search ensures we consider deeper paths first.
    for (node_count in (i - 1):1) {
      # If the path is found we can exit the loop
      if (path_found) {
        break
      }

      # Get the chain for the current path node
      chain <- get_chaine(list_path[[node_count]])


      ## Find exact attribute location
      # get the depth of the node
      xml_path <- paste0(chain, "/ATTRIBUTE[NAME='", attributes[i], "']")
      node_set <- XML::getNodeSet(main_tree, xml_path)
      node_set_length <- sapply(node_set, XML::xmlSize) |>
        length()

      # If the path exists
      if (node_set_length) {
        # Create the path to check against existing paths
        check_path <- c(list_path[[node_count]], attributes[i])

        # Check if the path is already in the list
        is_duplicate <- sapply(
          list_path,
          function(existing_path) {
            identical(check_path, existing_path)
          }
        ) |>
          any()

        # Use the negation of is_duplicate for the logic you need
        if (!is_duplicate) {
          path_found <- TRUE
          list_path[[i]] <- c(list_path[[node_count]], attributes[i])
        }
      }
    }
  }

  list_path
}


#' Process Leaf and Aggregated Nodes for Tree Construction
#'
#' An internal function that identifies and processes nodes within a tree that
#' are both \code{leaf} and \code{aggregated}. These specific nodes often play
#' unique roles in decision or aggregation trees, necessitating specialized
#' handling. The primary role of this function is to aid in the correct
#' construction and traversal of the tree when invoked by primary functions such
#' as \code{\link{create_tree}}.
#'
#' @param tree_nodes A \code{list} representing the tree nodes, where each
#'   element is an object of class \code{Node}.
#' @param leaves     A \code{character} vector detailing the names of the leaf
#'   nodes.
#' @param aggregated A \code{character} vector specifying the names of the
#'   aggregated nodes.
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{leaf_aggregated}: A \code{character} vector with names of nodes
#'         identified as both leaf and aggregated.
#'   \item \code{is_leaf_aggregated}: A \code{logical} indicating the existence
#'         of nodes that are both leaf and aggregated.
#'   \item \code{tree_nodes}: An updated \code{list} of tree nodes, where each
#'         element is an object of class \code{Node}, enriched with twin
#'         information and designations for leaf and aggregated nodes.
#' }
#'
#' @seealso \code{\link{create_tree}} for the primary function that constructs
#'   tree objects using objects of class \code{Node}.
#'
#' @keywords internal
process_aggregated_leaf <- function(tree_nodes, leaves, aggregated) {
  # Identify nodes that are both leaf and aggregated
  leaf_aggregated <- intersect(leaves, aggregated)

  # Check if there are any nodes that are both leaf and aggregated
  if (length(leaf_aggregated)) {
    is_leaf_aggregated <- TRUE
    for (i in seq_along(leaf_aggregated)) {
      # Get IDs corresponding to the leaf-aggregated node
      dup <- get_id(tree_nodes, leaf_aggregated[i])

      for (j in seq_along(dup)) {
        # Exclude the current ID and assign others as twins
        tree_nodes[[dup[j]]]@Twin <- dup[-which(dup == tree_nodes[[j]]@Id)]

        # Mark the node as both leaf and aggregated
        tree_nodes[[dup[j]]]@IsLeafAndAggregated <- TRUE
      }
    }
  } else {
    is_leaf_aggregated <- FALSE
  }

  return(list(
    leaf_aggregated = leaf_aggregated,
    is_leaf_aggregated = is_leaf_aggregated,
    tree_nodes = tree_nodes
  ))
}


#' Process Duplicate Leaf Nodes in Tree Structure
#'
#' @description An internal utility to identify multiple occurrences of leaf
#' attributes within a tree and mark them with the appropriate \code{Twin}
#' reference for differentiation. It also provides a comprehensive overview of
#' these occurrences, enhancing the process of tree traversal and analysis when
#' invoked by main functions like \code{\link{create_tree}}.
#'
#' @param tree_nodes A \code{list} of nodes, where each element is an object of
#'   class \code{Node}, representing the tree's hierarchical structure.
#' @param leaves A \code{character} vector detailing the names of leaf
#'   attributes within the tree.
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{multiple}: A \code{data.frame} or \code{matrix} presenting the
#'         count of occurrences for leaf attributes with more than one
#'         appearance.
#'   \item \code{is_multiple}: A \code{logical} indicating the presence of leaf
#'         attributes with multiple occurrences.
#'   \item \code{tree_nodes}: An updated \code{list} of tree nodes, where each
#'         element is an object of class \code{Node}, enriched with
#'         \code{Twin} information for duplicated leaf attributes.
#' }
#'
#' @seealso \code{\link{create_tree}} for the primary function that utilizes
#' this utility in tree construction.
#'
#' @keywords internal
process_duplicated_leaf <- function(tree_nodes, leaves) {
  # Create a table containing the count of occurrences for each leaf
  multiple <- table(leaves)
  is_multiple <- FALSE

  # If any leaf occurs more than once, process it
  if (max(multiple) > 1) {
    # Convert the table to a matrix and set column name
    multiple <- as.matrix(multiple[multiple > 1])
    colnames(multiple) <- "Occ"

    # Iterate through the multiple occurrences and set the 'Twin' reference
    for (i in 1:dim(multiple)[1]) {
      # Get the IDs corresponding to the leaf attribute
      dup <- get_id(tree_nodes, rownames(multiple)[i])

      # Set the 'Twin' reference for each duplicate
      for (j in 1:length(dup)) {
        # Find the index of tree_nodes[[dup[j]]]@Id within dup
        index <- which(dup == tree_nodes[[dup[j]]]@Id)

        # Remove that index from dup
        new_dup <- dup[-index]

        # Assign the resulting value to tree_nodes[[dup[j]]]@Twin
        tree_nodes[[dup[j]]]@Twin <- new_dup
      }
    }

    is_multiple <- TRUE
  } else {
    # If no multiple occurrences, set the 'Multiple' value to NA
    multiple <- data.frame(Occ = NA)
  }

  return(list(
    multiple = multiple,
    is_multiple = is_multiple,
    tree_nodes = tree_nodes
  ))
}


#' Construct a Node for a Decision Tree
#'
#' An internal function that constructs and populates a node for the decision
#' tree, capturing a variety of node attributes, including its relationships
#' with other nodes, its depth, status (leaf or aggregated), scale, and weight
#' information. It's one of the key utilities invoked by the main tree
#' construction function, \code{\link{create_tree}}.
#'
#' @param node_path A \code{character} vector representing the sequence of
#'   attributes leading to the node within the tree structure.
#' @param main_tree An XML object encapsulating the tree's hierarchical
#'   structure with the associated XML data.
#'
#' @return A new object of class \code{Node}, comprising the following elements:
#' \describe{
#'   \item{\code{Name}}{The name of the node, derived from the last element of
#'     \code{node_path}.}
#'   \item{\code{Depth}}{A \code{numeric} value indicating the depth of the node
#'     within the tree, derived from the length of \code{node_path}.}
#'   \item{\code{Twin}}{A \code{numeric} vector, initialized as empty, to store
#'     any potential twin nodes.}
#'   \item{\code{IsLeaf}}{A \code{logical} value denoting if the node is a
#'     terminal node without children.}
#'   \item{\code{IsLeafAndAggregated}}{A \code{logical} initialized to
#'     \code{FALSE} to flag nodes that serve as both leaf and aggregated nodes.}
#'   \item{\code{Mother}}{A \code{character} specifying the immediate parent of
#'     the node.}
#'   \item{\code{Sisters}}{A \code{character} vector enumerating nodes that
#'     share the same mother as the current node.}
#'   \item{\code{Children}}{A \code{character} vector listing all immediate
#'     child nodes of the current node.}
#'   \item{\code{Aggregation}}{The aggregation structure or method associated
#'     with the node.}
#'   \item{\code{RangeScale}}{A \code{numeric} value or range indicating the
#'     scale of the node's value.}
#'   \item{\code{ScaleLabel}}{A \code{character} vector representing the
#'     description or label of the node's scale.}
#'   \item{\code{Probability}}{A \code{numeric} vector detailing the probability
#'     or weight distribution associated with the node.}
#'   \item{\code{NodePath}}{A \code{character} vector mirroring the input
#'     \code{node_path} to trace the node's lineage.}
#' }

#'
#' @seealso \code{\link{create_tree}} for the primary function where this
#' utility function is engaged for tree generation.
#'
#' @keywords internal
create_node <- function(node_path, main_tree) {
  is_leaf <- determine_leaf_status(node_path, main_tree)
  children <- determine_children(node_path, main_tree, is_leaf)
  mother <- determine_mother(node_path)
  sisters <- determine_sisters(node_path, main_tree)
  scale_node <- determine_scale_node(node_path, main_tree)
  scale_label <- determine_scale_label(node_path, main_tree)
  aggregation <- determine_aggregation(node_path, main_tree, is_leaf, children)
  weight_list <- determine_weight_list(
    node_path, main_tree, is_leaf,
    aggregation, children
  )

  # Output
  out <- new("Node",
    Name = node_path[length(node_path)],
    Depth = length(node_path),
    Twin = numeric(0),
    IsLeaf = is_leaf,
    IsLeafAndAggregated = FALSE,
    Mother = mother,
    Sisters = sisters,
    Children = children,
    Aggregation = aggregation,
    RangeScale = scale_node,
    ScaleLabel = scale_label,
    Probability = weight_list,
    NodePath = node_path
  )
}


#' Determine the Leaf Status of a Node
#'
#' Determines whether a given node in the decision tree is a leaf or not. Being
#' a leaf node implies that the node does not have any children. This
#' distinction is crucial for the structural understanding of the tree and
#' further processing of the node.
#'
#' @param \code{node_path} A \code{character} vector representing the path to
#'   the node.
#' @param \code{main_tree} An XML object representing the main tree structure
#'   containing the XML data.
#'
#' @return A \code{logical} value indicating if the node is a leaf (TRUE) or not
#'   (FALSE).
#'
#' @keywords internal
determine_leaf_status <- function(node_path, main_tree) {
  # Construct the XML path
  xml_path <- paste0(get_chaine(node_path), "/FUNCTION")

  # Find nodes matching the path
  node_set <- XML::getNodeSet(main_tree, xml_path)

  # Check if any nodes were found
  has_nodes <- length(sapply(node_set, XML::xmlSize)) > 0

  # Return the opposite of has_nodes (since you want TRUE if no nodes are found)
  return(!has_nodes)
}



#' Determine the Children of a Node
#'
#' Identifies and retrieves the children of a specified node within the decision
#' tree. If the node is a leaf, then it won't have any children, and an empty
#' character vector will be returned. This distinction helps in the structural
#' understanding of the tree.
#'
#' @param \code{node_path} A \code{character} vector representing the path to
#'   the node.
#' @param \code{main_tree} An XML object representing the main tree structure
#'   containing the XML data.
#' @param \code{is_leaf} A \code{logical} value indicating if the node is a
#'   leaf.
#'
#' @return A \code{character} vector containing the names of the children.
#'   Returns an empty vector if the node is a leaf.
#'
#' @keywords internal
determine_children <- function(node_path, main_tree, is_leaf) {
  if (is_leaf) {
    vector(mode = "character", length = 0)
  } else {
    sapply(
      XML::getNodeSet(
        main_tree,
        paste0(get_chaine(node_path), "/ATTRIBUTE/NAME")
      ),
      XML::xmlValue
    )
  }
}


#' Determine the Mother of a Node
#'
#' Identifies the mother (or parent) of a specified node within the decision
#' tree. Knowing the parent node facilitates traversal and understanding of the
#' tree's hierarchical relationships. If the node is at the topmost level, it
#' won't have a mother, and NA will be returned.
#'
#' @param \code{node_path} A \code{character} vector representing the path to
#'   the node.
#'
#' @return A \code{character} value representing the name of the mother of the
#'   node. Returns \code{NA} if the node does not have a mother (topmost level).
#'
#' @keywords internal
determine_mother <- function(node_path) {
  if (length(node_path) > 1) {
    unlist(node_path[length(node_path) - 1])
  } else {
    as.character(NA)
  }
}



#' Determine the Sisters of a Node
#'
#' Identifies the sister nodes (nodes at the same hierarchical level under the
#' same parent) of a specified node within the decision tree. Sister nodes
#' provide insight into the horizontal relationships within the tree, which can
#' be important for certain types of analyses and tree traversals.
#'
#' @param \code{node_path} A \code{character} vector representing the path to
#'   the node.
#' @param \code{main_tree} An XML object representing the main tree structure.
#'
#' @return A \code{character} vector listing the names of the sister nodes.
#'   Returns an empty \code{character} vector if there are no sister nodes.
#'
#' @keywords internal
determine_sisters <- function(node_path, main_tree) {
  if (length(node_path) > 1) {
    sisters <- main_tree |>
      XML::getNodeSet(paste0(
        get_chaine(node_path[1:length(node_path) - 1]),
        "/ATTRIBUTE/NAME"
      )) |>
      sapply(XML::xmlValue)
  } else {
    sisters <- vector(mode = "character", length = 0)
  }

  sisters[sisters != node_path[length(node_path)]]
}


#' Determine the Scale of a Node
#'
#' Retrieves the scale associated with a given node within the decision tree.
#' The scale provides valuable insights into the node's dimension and can be
#' crucial for subsequent computations and visual representations.
#'
#' @param \code{node_path} A \code{character} vector representing the path to
#'   the node.
#' @param \code{main_tree} An XML object representing the main tree structure.
#'
#' @return An \code{integer} indicating the scale of the node. If no scale is
#'   found, the function returns 0.
#'
#' @keywords internal
determine_scale_node <- function(node_path, main_tree) {
  main_tree |>
    XML::getNodeSet(paste0(get_chaine(node_path), "/SCALE/SCALEVALUE")) |>
    sapply(XML::xmlSize) |>
    length()
}


#' Determine a Node's Scale Label
#'
#' Retrieves the descriptive scale label for a specific node in the decision
#' tree. This label can offer a more detailed understanding of the node's
#' dimension and is valuable for annotations and enhanced comprehension of the
#' node.
#'
#' @param \code{node_path} A \code{character} vector representing the path to
#'   the node.
#' @param \code{main_tree} An XML object representing the main tree structure.
#'
#' @return A \code{character} vector indicating the scale label of the node. If
#'   no scale label is found, the function returns an empty character vector.
#'
#' @keywords internal
determine_scale_label <- function(node_path, main_tree) {
  main_tree |>
    XML::getNodeSet(paste0(get_chaine(node_path), "/SCALE/SCALEVALUE/NAME")) |>
    sapply(XML::xmlValue)
}


#' Determine Node Aggregation
#'
#' Calculates the aggregation structure of a node. Essential for understanding
#' how information combines at the node level. Retrieves 'LOW' function values
#' from XML, transforms them, and determines scales using previous nodes.
#' Computes a factorial plan for nodes with multiple children, otherwise uses a
#' sequence.
#'
#' @param node_path \code{character} vector representing the path to the node.
#' @param main_tree Object containing the XML tree structure data.
#' @param is_leaf \code{logical} indicating if the node is a leaf.
#' @param children \code{list} of node's children.
#'
#' @return A matrix representing the aggregation structure. If not a leaf, it
#'   includes the factorial plan and transformed 'LOW' function values. For leaf
#'   nodes, the matrix contains zero.
#'
#' @keywords internal
determine_aggregation <- function(node_path, main_tree, is_leaf, children) {
  # Check if the node is not a leaf
  if (!is_leaf) {
    # Retrieve the low function values from the XML
    low_function_char <- main_tree |>
      XML::getNodeSet(paste0(get_chaine(node_path), "/FUNCTION/LOW")) |>
      sapply(XML::xmlValue) |>
      strsplit("")

    # Adjust the attribute from 0...n (char) to 1...n+1 (numeric)
    transformed_values <- as.numeric(low_function_char[[1]]) + 1

    # Determine the scales from nodes n-1 using a vectorized approach
    children_paths <- sapply(children, function(child) {
      get_chaine(c(node_path, child))
    })

    children_scales <- children_paths |>
      sapply(function(path) {
        main_tree |>
          XML::getNodeSet(paste0(path, "/SCALE/SCALEVALUE")) |>
          sapply(XML::xmlSize) |>
          length()
      })

    # Create the factorial plan if there's more than one child, otherwise use a
    # simple sequence
    if (length(children_scales) == 1) {
      aggregation <- seq_len(children_scales)
    } else {
      factorial_plan <- children_scales |>
        rev() |>
        AlgDesign::gen.factorial(center = FALSE) |>
        rev()

      # Convert the selected columns of the factorial plan into a matrix
      aggregation <- as.matrix(factorial_plan)
    }

    # Combine the aggregation matrix with the transformed values
    aggregation <- cbind(aggregation, transformed_values)

    # Set the column names for the aggregation matrix
    colnames(aggregation) <- c(children, node_path[length(node_path)])
  } else {
    # If the node is a leaf, set the aggregation to a matrix containing 0
    aggregation <- as.matrix(0)
  }

  return(aggregation)
}


#' Calculate Node Weight List
#'
#' Computes weights for a node based on its attributes and conditions, such as
#' its leaf status and child count. Weights can indicate probabilities or
#' significance within the tree.
#'
#' @param node_path \code{character} vector indicating the node's path.
#' @param main_tree Object containing the XML tree structure data.
#' @param is_leaf \code{logical} flag denoting if the node is a leaf.
#' @param aggregation Aggregation structure for potential weight determination.
#' @param children \code{list} of node's children.
#'
#' @return \code{numeric} vector of the node's associated weights.
#'
#' @keywords internal
determine_weight_list <- function(node_path, main_tree,
                                  is_leaf, aggregation, children) {
  # Initialize the weight list with the same size as the scale of the node
  weight_list <- numeric(determine_scale_node(node_path, main_tree))

  if (is_leaf) {
    # If the node is a leaf, assign equal weights
    weight_list <- rep(1 / length(weight_list), length(weight_list))
  } else if (length(children) == 1) {
    # If there's only one child, set the weight to 100
    weight_list <- 100
  } else if (main_tree |>
    XML::getNodeSet(paste0(
      get_chaine(node_path),
      "/FUNCTION/WEIGHTS"
    )) |>
    sapply(XML::xmlValue) |>
    length()) {
    # If specific weights are defined in the XML, retrieve and convert them
    weight_list <- main_tree |>
      XML::getNodeSet(paste0(get_chaine(node_path), "/FUNCTION/WEIGHTS")) |>
      sapply(XML::xmlValue) |>
      strsplit(";") |>
      unlist() |>
      as.numeric()
  } else {
    # If no specific condition is met, set the weight list to -1
    weight_list <- -1
  }

  return(weight_list)
}


#' Retrieve Node Chain
#'
#' Extracts the chain of nodes based on the provided node path.
#'
#' @param node_path \code{character} vector indicating the node's path.
#'
#' @return \code{character} representing the node chain.
#'
#' @keywords internal
get_chaine <- function(node_path) {
  for (k in 1:length(node_path)) {
    if (k == 1) {
      chaine <- paste0("//ATTRIBUTE[NAME='", node_path[k], "']")
    } else {
      chaine <- paste0(chaine, "/ATTRIBUTE[NAME='", node_path[k], "']")
    }
  }

  return(chaine)
}


#' Retrieve Node ID
#'
#' Extracts the ID associated with a specified node name from a provided list of
#' nodes.
#'
#' @param list_nodes A \code{list} of nodes where each node is an object with at
#'   least an \code{Id} and \code{Name} attribute.
#' @param node_name \code{character} specifying the name of the desired node.
#'
#' @return \code{numeric} ID of the node if found; \code{NULL} otherwise.
#'
#' @keywords internal
get_id <- function(list_nodes, node_name) {
  # Get the names of all nodes in the list
  node_names <- sapply(list_nodes, function(node) node@Name)

  # Find the indexes matching the given node name
  matching_index <- which(node_names == node_name)

  # If at least one matching index is found
  if (length(matching_index) > 0) {
    # Return the IDs of the matching nodes as a numeric vector
    return(sapply(matching_index, function(idx) as.numeric(list_nodes[[idx]]@Id)))
  } else {
    # Return NULL if no matching node is found
    return(NULL)
  }
}


#' Evaluate Order of Tree Nodes
#'
#' Determines the sequential order in which nodes within a tree should be
#' evaluated. This is particularly pertinent for trees with aggregated leaves,
#' where the evaluation order of the nodes impacts the final outcomes.
#'
#' @param tree a \code{Tree} object.
#'
#' @return A \code{numeric} vector indicating the order in which the nodes
#'   should be evaluated. An empty vector suggests no specific order.
#'
#' @details
#' The function first checks if the tree has aggregated leaves. If not,
#' it returns an empty vector since there is no explicit order needed.
#'
#' If there are aggregated leaves, the function sets an initial state
#' assuming none of the attributes are evaluated. It then marks the
#' tree's leaves as evaluated.
#'
#' The main logic revolves around iterating over leaf nodes and evaluating
#' them. However, if a node depends on another node, it is skipped and
#' re-evaluated in subsequent iterations. The function also contains a
#' safeguard against infinite loops by checking if the same nodes are
#' continually skipped across consecutive iterations.
#'
#' @seealso
#' Relevant functions and objects that can provide further insights:
#' \itemize{
#'   \item \code{\link{Tree-class}}: For more details on the Tree class.
#'   \item \code{\link{Node-class}}: For insights into the Node structure.
#' }
#'
#' @keywords internal
evaluate_order <- function(tree) {
  # Early exit if the tree doesn't have aggregated leaves, as there's no order
  # to evaluate
  if (!tree@IsLeafAggregated) {
    return(numeric(0))
  }

  # Set an initial state assuming none of the attributes have been evaluated
  # yet. We then immediately mark the tree's leaves as evaluated (with a value
  # of 1).
  results <- stats::setNames(rep(-1, tree@NumberOfAttributes), tree@Attributes)
  results[tree@Leaves] <- 1

  eval_order <- numeric(0) # Will store the final order of evaluation
  leaf_nodes <- tree@LeafAggregated

  # We'll iterate over leaf nodes, trying to evaluate them. However, some might
  # be dependent on others, so we might skip them for the current iteration and
  # retry in the next one.
  previous_nodes_to_skip <- NULL
  for (iteration in 1:length(leaf_nodes)) {
    nodes_to_skip <- numeric(0)

    # For each node, we'll check if its leaves have been evaluated. If they
    # haven't, it means this node might depend on another one and thus, we'll
    # skip it for now.
    for (i in seq_along(leaf_nodes)) {
      if (are_leaves_unevaluated(
        get_leaves(tree, leaf_nodes[i]),
        results,
        tree@Nodes
      )) {
        nodes_to_skip <- c(nodes_to_skip, leaf_nodes[i])
      } else {
        results[leaf_nodes[i]] <- 1

        # Add the node to the final evaluation order. In cases where a node has
        # multiple IDs, we're only interested in the non-leaf ones.
        node_id <- get_id(tree@Nodes, leaf_nodes[i])
        eval_order <- c(
          eval_order,
          Filter(function(id) !tree@Nodes[[id]]@IsLeaf, node_id)
        )
      }
    }

    # If we end up with the same nodes to skip in two consecutive iterations, it
    # means we've hit a cycle or a dead-end and should break to avoid an
    # infinite loop.
    if (identical(nodes_to_skip, previous_nodes_to_skip)) {
      break
    }

    previous_nodes_to_skip <- nodes_to_skip
    leaf_nodes <- nodes_to_skip
  }

  return(eval_order)
}


#' Check if Any Leaves are Unevaluated
#'
#' Determines if any of the provided leaves within a \code{Tree} have not yet
#' been evaluated. A leaf is deemed unevaluated if its corresponding value in
#' the results vector is -1.
#'
#' @param leaves A \code{character} vector listing the leaves to be checked.
#' @param results A named \code{numeric} vector, where names correspond to
#'   \code{Node} names and values indicate the evaluation status of the node. A
#'   value of -1 implies the \code{Node} is unevaluated.
#' @param nodes A list of \code{Node}s, where each entry contains details of a
#'   node.
#'
#' @return A \code{logical}. Returns `TRUE` if any of the leaves are yet to be
#'   evaluated; otherwise, it returns `FALSE`.
#'
#' @seealso Related functions and objects that might be of interest:
#' \itemize{
#'   \item \code{\link{Tree-class}}: For a more detailed understanding of the
#'     \code{Tree} class.
#'   \item \code{\link{Node-class}}: To delve deeper into the structure of a
#'     \code{Node}.
#'   \item \code{\link{evaluate_order}}: A function that determines the sequence
#'     for evaluating \code{Node}s in a \code{Tree}.
#' }
#'
#' @keywords internal
are_leaves_unevaluated <- function(leaves, results, nodes) {
  any(sapply(leaves, function(leaf) results[nodes[[leaf]]@Name] == -1))
}


#' Retrieve Leaves Associated with a Given Node in a Tree
#'
#' Extracts the leaf IDs associated with a specified \code{Node} from a given
#' \code{Tree}.
#'
#' @param tree a \code{Tree} object.
#' @param node_id A \code{numeric} or \code{character} representing the
#'   identifier or the name of the \code{Node} for which leaves are to be
#'   retrieved.
#'
#' @return A \code{numeric} vector containing the IDs of the leaves associated
#'   with the specified \code{Node}.
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
#' @keywords internal
get_leaves <- function(tree, node_id) {
  # If node_id is character, convert it to numeric
  if (is.character(node_id)) {
    leaf_id <- get_id(tree@Nodes, node_id)
    node_id <- unlist(lapply(leaf_id, function(x) {
      if (!tree@Nodes[[x]]@IsLeaf) tree@Nodes[[x]]@Id
    }))
  }

  # Fetch nodes whose path matches the specified node's path
  leaf_nodes <- unlist(lapply(tree@Nodes, function(x) {
    if (length(grep(
      paste(tree@Nodes[[node_id]]@NodePath, collapse = ""),
      paste(x@NodePath, collapse = "")
    ))) {
      x@Id
    }
  }))

  # Filter out the leaves from the fetched nodes and return their IDs
  leaves_ids <- unlist(lapply(leaf_nodes, function(x) {
    if (tree@Nodes[[x]]@IsLeaf) tree@Nodes[[x]]@Id
  }))

  return(leaves_ids)
}
