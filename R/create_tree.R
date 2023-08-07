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
    tree_nodes <- lapply(list_path, createNode, main_tree = main_tree)
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
      chain <- getChaine(list_path[[node_count]])

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
      dup <- getID(tree_nodes, leaf_aggregated[i])

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
      dup <- getID(tree_nodes, rownames(multiple)[i])

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



#' Create a node for the decision tree
#'
#' @param listeNoeuds A list of nodes
#' @param main_tree The main tree (an XML object)
#'
#' @return A new Node object
#'
#' @export
createNode <- function(listeNoeuds, main_tree) {

  # Is it a leaf?
  isLeaf <- ifelse(XML::getNodeSet(main_tree, paste0(getChaine(listeNoeuds),
                                                     "/FUNCTION")) %>%
                     sapply(XML::xmlSize) %>%
                     length(),
                   F, T)
  # Children
  l.Children <- if (isLeaf) {
    vector(mode="character",length=0)
  } else {
    sapply(XML::getNodeSet(main_tree, paste0(getChaine(listeNoeuds),
                                             "/ATTRIBUTE/NAME")),
           XML::xmlValue)
  }

  # Mother if any
  mother<-ifelse(length(listeNoeuds) > 1,
                 listeNoeuds[length(listeNoeuds) - 1],
                 character(0))

  # Sisters: they do have the same mother
  l.Sisters <- if (length(listeNoeuds)>1) {
    main_tree %>%
      XML::getNodeSet(paste0(getChaine(listeNoeuds[1:length(listeNoeuds)-1]),
                             "/ATTRIBUTE/NAME")) %>%
      sapply(XML::xmlValue)
  } else {vector(mode = "character", length = 0)}
  l.Sisters <- l.Sisters[l.Sisters[] != listeNoeuds[length(listeNoeuds)]]

  # Scale and labels
  scaleNode <- main_tree %>%
    XML::getNodeSet(paste0(getChaine(listeNoeuds), "/SCALE/SCALEVALUE")) %>%
    sapply(XML::xmlSize) %>%
    length()

  scaleLabel <- main_tree %>%
    XML::getNodeSet(paste0(getChaine(listeNoeuds),
                           "/SCALE/SCALEVALUE/NAME")) %>%
    sapply(XML::xmlValue)

  # Aggregation function
  if (!isLeaf) {
    c.Function <- main_tree %>%
      XML::getNodeSet(paste0(getChaine(listeNoeuds), "/FUNCTION/LOW")) %>%
      sapply(XML::xmlValue)

    # Transform as a vector
    nbChar <- nchar(c.Function)
    v.Function <- numeric(nbChar)
    for(i in 1:nbChar) {
      # Modify attribute 0...n to 1...n+1
      v.Function[i] <- as.numeric(substr(c.Function, i, i)) + 1
    }

    # Scales from nodes n-1
    nbChildren <- length(l.Children)
    scaleChildren <- numeric(nbChildren)
    for(i in 1:nbChildren) {
      scaleChildren[i] <- main_tree %>%
        XML::getNodeSet(paste0(getChaine(c(listeNoeuds,l.Children[i])),
                               "/SCALE/SCALEVALUE")) %>%
        sapply(XML::xmlSize) %>%
        length()
    }

    # Create the factorial plan
    if(nbChildren == 1) {
      aggregation <- (1:scaleChildren)
    } else {
      factorialPlan <- scaleChildren %>%
        as.numeric() %>%
        rev() %>%
        AlgDesign::gen.factorial(center = FALSE) %>%
        rev()

      nbFactorialPlan <- dim(factorialPlan)[1]
      aggregation <- as.matrix(factorialPlan[, seq(ncol(factorialPlan))])
    }
    aggregation <- cbind(aggregation, v.Function)
    colnames(aggregation) <- c(l.Children, listeNoeuds[length(listeNoeuds)])
  } else {aggregation <- as.matrix(0)}

  # Create the weights (equal weights if not defined)
  WeightList <- numeric(scaleNode)

  if (isLeaf) {
    WeightList <- rep(1/scaleNode,scaleNode)
  } else if (nbChildren==1) {
    WeightList <- 100
  } else if (main_tree %>%
             XML::getNodeSet(paste0(getChaine(listeNoeuds),
                                    "/FUNCTION/WEIGHTS")) %>%
             sapply(XML::xmlValue) %>%
             length()) {

    WeightList <- main_tree %>%
      XML::getNodeSet(paste0(getChaine(listeNoeuds),
                             "/FUNCTION/WEIGHTS")) %>%
      sapply(XML::xmlValue) %>%
      strsplit(";") %>%
      unlist() %>%
      as.numeric()

  } else { WeightList <- -1 }

  # Output
  out <- new("Node",
             Name = listeNoeuds[length(listeNoeuds)],
             Depth = length(listeNoeuds),
             Twin = numeric(0),
             IsLeaf = isLeaf,
             IsLeafAndAggregated = FALSE,
             Mother = mother,
             Sisters = l.Sisters,
             Children = l.Children,
             Aggregation = aggregation,
             RangeScale = scaleNode,
             ScaleLabel = scaleLabel,
             Probability = WeightList,
             NodePath = listeNoeuds)
}


#' Get the chain of nodes
#'
#' Retrieves the chain of nodes from a given list of nodes.
#'
#' @param listeNoeuds A list of nodes
#'
#' @return A string representing the chain of nodes
#'
#' @export
getChaine <- function(listeNoeuds) {

  for(k in 1:length(listeNoeuds)) {
    if (k==1) {
      chaine <- paste0("//ATTRIBUTE[NAME='", listeNoeuds[k], "']")
    } else {
      chaine <- paste0(chaine, "/ATTRIBUTE[NAME='", listeNoeuds[k], "']")
    }
  }

  return(chaine)
}


#' Get the ID of a given node
#'
#' Retrieves the ID of a given node from a list of nodes.
#'
#' @param listNodes A list of nodes
#' @param nodeName The name of the node
#'
#' @return The ID of the node
#'
#' @export
# getID <- function(listNodes,nodeName) {
#     out <- numeric(0)
#     for(i in 1:length(listNodes)) {
#         if(rev(listNodes[[i]]@NodePath)[1] == nodeName)
#             out <- c(out, listNodes[[i]]@Id)
#     }
#     return(out)
# }
getID <- function(listNodes,nodeName) {
  out <- numeric(0)
  for(i in 1:length(listNodes)) {
    nodePathLength <- length(listNodes[[i]]@NodePath)
    if(listNodes[[i]]@NodePath[nodePathLength] == nodeName)
      out <- c(out,listNodes[[i]]@Id)
  }
  return(out)
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
          id <- getID(aTree@Nodes, l.Nodes[i])
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
    l.id <- getID(aTree@Nodes, nodeID)
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
