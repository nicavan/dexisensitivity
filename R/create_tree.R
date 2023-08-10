#' Create a Tree from DEXi's XML output
#'
#' Takes the XML output from a DEXi model and constructs a tree structure.
#' The function iterates through each root attribute in the XML and constructs
#' tree objects representing the hierarchical decision model.
#'
#' @param main_tree An XML object representing the main tree structure. It must
#'   be a valid XML object containing the DEXi decision model.
#'
#' @return A list of tree objects, each representing a distinct root attribute
#'   within the DEXi XML structure. Each tree object contains details about the
#'   root attribute, nodes, leaves, depth, and other related information
#'   regarding the structure of the decision model.
#'
#' @import XML
#' @import AlgDesign
#'
#' @seealso
#' \code{\link{process_aggregated_leaf}},
#'  \code{\link{process_duplicated_leaf}},
#'   \code{\link{get_dexi_attributes}},
#'    \code{\link{get_paths}}
#'
#' @export
create_tree <- function(main_tree) {

  # Get root(s) name(s) of the DEXi Tree
  list_root_name <- sapply(XML::getNodeSet(doc = main_tree,
                                           path = "/DEXi/ATTRIBUTE/NAME"),
                           XML::xmlValue)
  list_tree <- list()

  # For each Tree's roots
  for(i_root_name in seq_along(list_root_name)) {

    root_name <- list_root_name[i_root_name]
    attributes <- get_dexi_attributes(main_tree, root_name)
    number_of_attributes <- length(attributes)
    list_path <- get_paths(attributes, main_tree, root_name)

    # Creates the nodes
    tree_nodes <- lapply(list_path, create_node, main_tree = main_tree)
    for(i in 1:number_of_attributes) {
      tree_nodes[[i]]@Id <- i
    }

    # Maximum tree depth
    number_of_levels <- max(sapply(list_path, length))

    # List of leaves
    leaves <- unlist(sapply(tree_nodes,
                            function(x) {if(x@IsLeaf) x@Name}))

    # List of Aggregated
    aggregated <- unlist(sapply(tree_nodes,
                                function(x) {if(!x@IsLeaf) x@Name}))

    # Leaf-Aggregated attribute
    leaf_aggregated_result <- process_aggregated_leaf(tree_nodes,
                                                      leaves,
                                                      aggregated)
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
    if(is_leaf_aggregated) {
      leaves <- leaves[-c(which(is.element(leaves,
                                           leaf_aggregated)))]
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
                Paths = list_path)

    tree@EvaluationOrder <- EvaluateOrder(tree)

    list_tree[[i_root_name]] <- tree
  }

  return(list_tree)
}


#' Retrieve Attribute Names from a DEXi XML Tree
#'
#' Retrieves all the attribute names under a specified root node within a DEXi
#' XML tree structure. It forms a search path within the XML tree to fetch all
#' child attributes starting from the given root name.
#'
#' @param main_tree An XML object representing the main tree structure. It must
#'   be a valid XML object containing the DEXi decision model.
#' @param root_name A character representing the name of the root attribute from
#'   which to start the search. This should correspond to an existing attribute
#'   name within the XML structure.
#'
#' @return A character vector containing the names of the root attribute and all
#'   its descendant attributes. The vector starts with the name of the root
#'   attribute followed by its child attributes in the order they appear in the
#'   XML tree.
get_dexi_attributes <- function(main_tree, root_name) {
  to_search <- paste0("//ATTRIBUTE[NAME='", root_name, "']//ATTRIBUTE/NAME")
  attributes <- c(root_name,
                  XML::xmlValue(XML::getNodeSet(main_tree, to_search)))
}


#' Retrieve Attribute Paths in an XML Tree
#'
#' Traverses an XML tree (representing a hierarchical structure) to find
#' specific paths for a given list of attributes, starting from a specified root
#' name.
#'
#' @param attributes   A character vector containing the names of the attributes
#'   for which to find paths.
#' @param main_tree    An XML object representing the tree within which to
#'   search for paths.
#' @param root_name    A character representing the name of the root attribute
#'   from which to begin the search.
#'
#' @return             A list of paths, where each path is a character vector
#'   representing the sequence of attributes from the root name to the target
#'   attribute.
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
      if (path_found) {break}

      # Get the chain for the current path node
      chain <- get_chaine(list_path[[node_count]])

      # Search in the main_tree for the attribute. We are looking for the
      # attribute in the XML tree to find its exact location.
      if (XML::getNodeSet(main_tree,
                          paste0(chain, "/ATTRIBUTE[NAME='",
                                 attributes[i], "']")) %>%
          sapply(XML::xmlSize) %>%
          length()) {

        # We check that the current path has not already been identified to
        # prevent duplicates.
        if (list_path %>%
            lapply(function(x) {
              identical(c(list_path[[node_count]], attributes[i]), x)
            }) %>%
            unlist() %>%
            sum() %>%
            `!`) {
          path_found <- TRUE
          list_path[[i]] <- c(list_path[[node_count]], attributes[i])
        }
      }
    }
  }

  return(list_path)
}


#' Process Leaf and Aggregated Nodes
#'
#' Identifies and processes the nodes in the tree that are both leaf and
#' aggregated. Such nodes can have specific roles in decision or aggregation
#' trees and may require special treatment. By identifying these nodes, the
#' function aids in properly handling them in the overall evaluation or
#' traversal of the tree.
#'
#' @param tree_nodes A list of tree nodes.
#' @param leaves A vector containing the names of leaf nodes.
#' @param aggregated A vector containing the names of aggregated nodes.
#'
#' @return A list with the following elements:
#'   \itemize{
#'     \item \code{leaf_aggregated}: A vector containing the names of nodes
#'     that are both leaf and aggregated.
#'     \item \code{is_leaf_aggregated}: A logical value indicating if such nodes
#'     exist.
#'     \item \code{tree_nodes}: A list of tree nodes updated with the twin
#'     information and markings for leaf and aggregated nodes.
#'   }
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

  return(list(leaf_aggregated = leaf_aggregated,
              is_leaf_aggregated = is_leaf_aggregated,
              tree_nodes = tree_nodes))
}


#' Process Duplicated leaf
#'
#' Identifies the multiple occurrences of leaf attributes in the tree structure,
#' marking them with the appropriate `Twin` reference. It also returns a data
#' frame containing the count of occurrences.
#'
#' @param tree_nodes A list of nodes in the tree structure.
#' @param leaves A character vector containing the names of leaf attributes.
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item `multiple`: A data frame or matrix containing the count of
#'   occurrences of leaf attributes.
#'   \item `is_multiple`: A logical value indicating whether there are multiple
#'   occurrences of any leaf attributes.
#'   \item \code{tree_nodes}: A list of tree nodes updated with the twin
#'     information and markings for duplicated leaf.
#' }
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

  return(list(multiple = multiple,
              is_multiple = is_multiple,
              tree_nodes = tree_nodes))
}


#' Create a Node for the Decision Tree
#'
#' Constructs a new node for the decision tree, incorporating various attributes
#' and details including leaf status, children, mother, sisters, scale, scale
#' label, aggregation, and weight list.
#'
#' @param node_path A character vector representing the path to the node.
#' @param main_tree An XML object representing the main tree structure
#'   containing the XML data.
#'
#' @return A new "Node" object containing the following components:
#' \describe{
#'   \item{Name}{The name of the node.}
#'   \item{Depth}{The depth of the node within the tree.}
#'   \item{Twin}{Numeric value representing a twin node, initialized to 0.}
#'   \item{IsLeaf}{Boolean indicating if the node is a leaf.}
#'   \item{IsLeafAndAggregated}{Boolean, initialized to FALSE.}
#'   \item{Mother}{Character representing the mother of the node.}
#'   \item{Sisters}{Character vector representing the sisters of the node.}
#'   \item{Children}{Character vector representing the children of the node.}
#'   \item{Aggregation}{Aggregation structure for the node.}
#'   \item{RangeScale}{The range scale of the node.}
#'   \item{ScaleLabel}{Character vector representing the scale label of the
#'   node.}
#'   \item{Probability}{Numeric vector representing the probability or weights
#'   for the node.}
#'   \item{NodePath}{Character vector representing the path to the node.}
#' }
#'
#' @export
create_node <- function(node_path, main_tree) {

  is_leaf <- determine_leaf_status(node_path, main_tree)
  children <- determine_children(node_path, main_tree, is_leaf)
  mother <- determine_mother(node_path)
  sisters <- determine_sisters(node_path, main_tree)
  scale_node <- determine_scale_node(node_path, main_tree)
  scale_label <- determine_scale_label(node_path, main_tree)
  aggregation <- determine_aggregation(node_path, main_tree, is_leaf, children)
  weight_list <- determine_weight_list(node_path, main_tree, is_leaf,
                                       aggregation, children)

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
             NodePath = node_path)
}


#' Identify if a Node is a Leaf
#'
#' Essential to understand the tree structure and facilitates further
#' decision-making about the node.
#'
#' @param node_path A character vector representing the path to the node.
#' @param main_tree Main tree structure containing the XML data.
#'
#' @return Boolean indicating if the node is a leaf.
#'
#' @keywords internal
determine_leaf_status <- function(node_path, main_tree) {
  if (XML::getNodeSet(main_tree,
                      paste0(get_chaine(node_path), "/FUNCTION")) %>%
      sapply(XML::xmlSize) %>%
      length()) {
    FALSE
  } else {
    TRUE
  }
}


#' Find the Children of a Node
#'
#' Enhances readability by encapsulating the logic for understanding the node's
#' offspring.
#'
#' @param node_path A character vector representing the path to the node.
#' @param main_tree Main tree structure containing the XML data.
#' @param is_leaf Boolean indicating if the node is a leaf.
#'
#' @return A list of children.
#'
#' @keywords internal
determine_children <- function(node_path, main_tree, is_leaf) {
  if (is_leaf) {
    vector(mode = "character", length = 0)
  } else {
    sapply(XML::getNodeSet(main_tree,
                           paste0(get_chaine(node_path), "/ATTRIBUTE/NAME")),
           XML::xmlValue)
  }
}


#' Identify the Mother of a Node
#'
#' Contributes to understanding the relationship between nodes and navigation
#' within the tree's structure.
#'
#' @param node_path A character vector representing the path to the node.
#'
#' @return Mother of the node if exists.
#'
#' @keywords internal
determine_mother <- function(node_path) {
  if (length(node_path) > 1) {
    unlist(node_path[length(node_path) - 1])
  } else {
    as.character(NA)
  }
}


#' Find the Sisters of a Node
#'
#' Aids in understanding horizontal relationships within the tree, crucial for
#' some analyses.
#'
#' @param node_path A character vector representing the path to the node.
#' @param main_tree Main tree structure containing the XML data.
#'
#' @return A list of sisters.
#'
#' @keywords internal
determine_sisters <- function(node_path, main_tree) {
  if (length(node_path) > 1) {
    sisters <- main_tree %>%
      XML::getNodeSet(paste0(get_chaine(node_path[1:length(node_path) - 1]),
                             "/ATTRIBUTE/NAME")) %>%
      sapply(XML::xmlValue)
  } else {
    sisters <- vector(mode = "character", length = 0)
  }

  sisters[sisters != node_path[length(node_path)]]
}


#' Determine the Scale of a Node
#'
#' Used to understand the node's working scale, which might be necessary for
#' subsequent calculations or visualization.
#'
#' @param node_path A character vector representing the path to the node.
#' @param main_tree Main tree structure containing the XML data.
#'
#' @return Scale of the node.
#'
#' @keywords internal
determine_scale_node <- function(node_path, main_tree) {
  main_tree %>%
    XML::getNodeSet(paste0(get_chaine(node_path), "/SCALE/SCALEVALUE")) %>%
    sapply(XML::xmlSize) %>%
    length()
}


#' Determine a Node's Scale Label
#'
#' Useful for annotating or understanding the node at a more descriptive level.
#'
#' @param node_path A character vector representing the path to the node.
#' @param main_tree Main tree structure containing the XML data.
#'
#' @return Scale label of the node.
#'
#' @keywords internal
determine_scale_label <- function(node_path, main_tree) {
  main_tree %>%
    XML::getNodeSet(paste0(get_chaine(node_path), "/SCALE/SCALEVALUE/NAME")) %>%
    sapply(XML::xmlValue)
}


#' Determine Aggregation for a Node
#'
#' crucial for understanding how information is combined at the node level. It
#' contains the logic for aggregation based on whether the node is a leaf and
#' other properties. The function retrieves the low function values from the XML
#' and adjusts them to numeric format. Then, it determines the scales from nodes
#' n-1 and creates the factorial plan or sequence depending on the number of
#' children. If the node is a leaf, it returns a matrix with zero.
#'
#' @param node_path A character vector representing the path to the node.
#' @param main_tree Main tree structure containing the XML data.
#' @param is_leaf Boolean indicating if the node is a leaf.
#' @param children List of children nodes.
#'
#' @return If the node is not a leaf, the function returns the aggregation
#'   structure, including a matrix representing the factorial plan and
#'   transformed low function values. If the node is a leaf, the function
#'   returns a matrix with zero.
#'
#' @keywords internal
determine_aggregation <- function(node_path, main_tree, is_leaf, children) {
  # Check if the node is not a leaf
  if (!is_leaf) {

    # Retrieve the low function values from the XML
    low_function_char <- main_tree %>%
      XML::getNodeSet(paste0(get_chaine(node_path), "/FUNCTION/LOW")) %>%
      sapply(XML::xmlValue) %>%
      strsplit("")

    # Adjust the attribute from 0...n (char) to 1...n+1 (numeric)
    transformed_values <- as.numeric(low_function_char[[1]]) + 1

    # Determine the scales from nodes n-1 using a vectorized approach
    children_paths <- sapply(children, function(child) {
      get_chaine(c(node_path, child))
      })

    children_scales <- children_paths %>%
      sapply(function(path) {
        main_tree %>%
          XML::getNodeSet(paste0(path, "/SCALE/SCALEVALUE")) %>%
          sapply(XML::xmlSize) %>%
          length()
      })

    # Create the factorial plan if there's more than one child, otherwise use a
    # simple sequence
    if (length(children_scales) == 1) {
      aggregation <- seq_len(children_scales)
    } else {
      factorial_plan <- children_scales %>%
        rev() %>%
        AlgDesign::gen.factorial(center = FALSE) %>%
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


#' Determine Weight List for a Node
#'
#' Calculates the weights associated with a particular node based on
#' various conditions, such as whether the node is a leaf and the
#' number of children. Weights may represent probabilities or importance
#' factors in the overall tree structure.
#'
#' @param node_path A character vector representing the path to the node.
#' @param main_tree Main tree structure containing the XML data.
#' @param is_leaf Boolean indicating if the node is a leaf.
#' @param aggregation Aggregation structure, possibly used in the calculation.
#' @param children List of children nodes.
#'
#' @return A numeric vector containing the weights for the node.
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
  } else if (main_tree %>%
             XML::getNodeSet(paste0(get_chaine(node_path),
                                    "/FUNCTION/WEIGHTS")) %>%
             sapply(XML::xmlValue) %>%
             length()) {
    # If specific weights are defined in the XML, retrieve and convert them
    weight_list <- main_tree %>%
      XML::getNodeSet(paste0(get_chaine(node_path), "/FUNCTION/WEIGHTS")) %>%
      sapply(XML::xmlValue) %>%
      strsplit(";") %>%
      unlist() %>%
      as.numeric()
  } else {
    # If no specific condition is met, set the weight list to -1
    weight_list <- -1
  }

  return(weight_list)
}


#' Get the chain of nodes
#'
#' Retrieves the chain of nodes from a given node path.
#'
#' @param node_path A character vector representing the path to the node.
#'
#' @return A string representing the chain of nodes
#'
#' @export
get_chaine <- function(node_path) {

  for(k in 1:length(node_path)) {
    if (k==1) {
      chaine <- paste0("//ATTRIBUTE[NAME='", node_path[k], "']")
    } else {
      chaine <- paste0(chaine, "/ATTRIBUTE[NAME='", node_path[k], "']")
    }
  }

  return(chaine)
}


#' Get the ID of a given node
#'
#' Retrieves the ID of a given node from a list of nodes.
#'
#' @param list_nodes A list of nodes
#' @param node_name The name of the node
#'
#' @return The ID of the node
#'
#' @export
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


#' Evaluate the order of nodes in a tree
#'
#' @param aTree A tree
#'
#' @return The evaluated order of nodes
#'
#' @export
EvaluateOrder <- function(aTree) {
  evalOrder <- numeric(0)
  if (aTree@IsLeafAggregated) {

    results <- numeric(aTree@NumberOfAttributes)
    names(results) <- aTree@Attributes
    results[] <- -1
    results[aTree@Leaves] <- 1
    l.Nodes <- aTree@LeafAggregated
    isDone <- F

    while (!isDone) {
      skip <- numeric(0)

      for(i in 1:length(l.Nodes)) {
        # In case of interelated subTrees ..
        # .. need to find the first one without LeafAggregated Leaves
        l.Leaves <- getLeaves(aTree, l.Nodes[i])
        if(l.Leaves %>%
           sapply(function(x) {
             ifelse(results[aTree@Nodes[[x]]@Name] == -1, 1, 0)
           }) %>%
           unlist() %>%
           sum()) {
          skip <- c(skip,l.Nodes[i])
        } else {
          results[l.Nodes[i]] <- 1
          id <- get_id(aTree@Nodes, l.Nodes[i])
          if(length(id) > 1)
            id <- unlist(sapply(id, function(x) {
              if(!aTree@Nodes[[x]]@IsLeaf) {x}
            }))
          evalOrder <- c(evalOrder, id)
        }
      }

      if(!length(skip)) {
        isDone <- T
      } else {
        l.Nodes <- skip
      }

    }
  }
  return(evalOrder)
}


#' Get the leaves of a given node
#'
#' Retrieves the leaves of a given node
#'
#' @param aTree A Tree
#' @param nodeID The ID of the node
#'
#' @return The leaves of the node
#'
#' @export
getLeaves <- function(aTree,nodeID) {
  if (is.character(nodeID)) {
    l.id <- get_id(aTree@Nodes, nodeID)
    nodeID <- unlist(sapply(l.id, function(x) {
      if(!aTree@Nodes[[x]]@IsLeaf)aTree@Nodes[[x]]@Id
    }))
  }

  l.Nodes <- unlist(sapply(aTree@Nodes, function(x) {
    if(length(grep(paste(aTree@Nodes[[nodeID]]@NodePath, collapse = ""),
                   paste(x@NodePath, collapse = ""))))x@Id
  }))

  return(unlist(sapply(l.Nodes, function(x) {
    if (aTree@Nodes[[x]]@IsLeaf) {aTree@Nodes[[x]]@Id}
  })))
}
