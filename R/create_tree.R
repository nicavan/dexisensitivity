#' Create a Tree
#'
#' Use the DEXi's XML output to create the Tree.
#'
#' @param MT Tree model
#'
#' @return
#' @export
#'
#' @import XML
#' @import AlgDesign
#' @examples
createTree <- function(MT) {

    # Get root(s) name(s) of the DEXi Tree
    listRootName <- sapply(XML::getNodeSet(doc = MT,
                                           path = "/DEXi/ATTRIBUTE/NAME"),
                           XML::xmlValue)
    listTree <- list()

    # For each Tree's roots
    for(iRootName in 1:length(listRootName)) {

        # Get root name
        rootName <- listRootName[iRootName]

        # List of the attributes names
        to.search <- paste0("//ATTRIBUTE[NAME='",rootName,"']//ATTRIBUTE/NAME")
        l.Attrib <- c(rootName, XML::xmlValue(XML::getNodeSet(MT, to.search)))
        nbAttrib <- length(l.Attrib)

        # List of Paths
        listPath <- list()
        listPath[[1]] <- rootName
        for(i in 2:nbAttrib) {
            nbNoeuds <- (i-1)
            isOK <- F
            while (!isOK) {
                # Get previous node chain
                chaine <- getChaine(listPath[[nbNoeuds]])

                #
                if (XML::getNodeSet(MT, paste0(chaine, "/ATTRIBUTE[NAME='",
                                               l.Attrib[i], "']")) %>%
                    sapply(XML::xmlSize) %>%
                    length()) {

                    # Need to check if not already in the list.
                    # This may happen in some structure !!!
                    # ex : duplicates in the same node.
                    if (listPath %>%
                        sapply(function(x) {
                            identical(c(listPath[[nbNoeuds]], l.Attrib[i]), x)
                        }) %>%
                        unlist() %>%
                        sum() %>%
                        `!`) {

                        isOK <- T
                        listPath[[i]] <- c(listPath[[nbNoeuds]],l.Attrib[i])

                    } else {nbNoeuds <- nbNoeuds-1}

                } else {nbNoeuds <- nbNoeuds-1}
            }
        }

        # Creates the nodes
        TreeNodes <- vector(nbAttrib, mode = "list")
        TreeNodes <- lapply(listPath, createNode, MT = MT)
        for(i in 1:nbAttrib) {
            TreeNodes[[i]]@id <- i
            TreeNodes[[i]]@isLeafAndAggregated <- F
            TreeNodes[[i]]@Twin <- numeric(0)
        }

        # Maximum tree depth
        nbLevels <- max(sapply(listPath, length))

        # List of leaves
        l.Leaves <- unlist(sapply(TreeNodes,
                                  function(x) {if(x@isLeaf) x@name}))
        nbLeaves<-length(l.Leaves)

        # List of Aggregated
        l.Aggregated <- unlist(sapply(TreeNodes,
                                      function(x) {if(!x@isLeaf) x@name}))
        nbAggregated <- length(l.Aggregated)

        # Leaf-Aggregated attribute
        l.LeafAggregated <- intersect(l.Leaves, l.Aggregated)
        if(length(l.LeafAggregated)) {
            isLeafAggregated <- T
            for(i in 1:length(l.LeafAggregated)) {
                dup <- getID(TreeNodes, l.LeafAggregated[i])
                for(j in 1:length(dup)) {
                    TreeNodes[[dup[j]]]@Twin <- dup[-c(which(is.element(dup, TreeNodes[[j]]@id)))]
                    TreeNodes[[dup[j]]]@isLeafAndAggregated <- T
                }
            }
        } else {isLeafAggregated <- F}



        # Check for duplicated leaves
        Multiple <- table(l.Leaves)
        if(max(Multiple)>1) {
            Multiple <- as.matrix(Multiple[Multiple > 1])
            colnames(Multiple)<-"Occ"
            for(i in 1:dim(Multiple)[1]) {
                dup <- getID(TreeNodes, rownames(Multiple)[i])
                for(j in 1:length(dup)) {
                    TreeNodes[[dup[j]]]@Twin <- dup[-c(which(is.element(dup, TreeNodes[[dup[j]]]@id)))]
                }
            }
            isMultiple <- T
        } else {
            Multiple <- data.frame(Occ = NA)
            isMultiple <- F
        }

        # Get the true leaves
        l.Leaves <- unique(l.Leaves) #Remove duplicates
        if(isLeafAggregated) {
            l.Leaves <- l.Leaves[-c(which(is.element(l.Leaves,
                                                     l.LeafAggregated)))]
        }
        nbLeaves <- length(l.Leaves)

        # Define the order to evaluate a tree if LeafAggregated node
        aTree <- new("Tree",
                     rootName = rootName,
                     nbAttributes = nbAttrib,
                     nbLeaves = nbLeaves,
                     Depth = nbLevels,
                     Nodes = TreeNodes,
                     Multiple = as.data.frame(Multiple),
                     isMultiple = isMultiple,
                     isLeafAggregated = isLeafAggregated,
                     LeafAggregated = l.LeafAggregated,
                     Attributes = l.Attrib,
                     Leaves = l.Leaves,
                     Aggregated = l.Aggregated,
                     EvalOrder = numeric(0),
                     Paths = listPath)

        aTree@EvalOrder <- EvaluateOrder(aTree)

        listTree[[iRootName]] <- aTree
    }

    return(listTree)
}



#' Title
#'
#' @param listeNoeuds node list
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @param listeNoeuds Node list
#'
#' @param MT Main Tree
#'
#' @return
#' @export
#'
#' @examples
createNode <- function(listeNoeuds, MT) {

    # Is it a leaf?
    isLeaf <- ifelse(XML::getNodeSet(MT, paste0(getChaine(listeNoeuds),
                                                "/FUNCTION")) %>%
                         sapply(XML::xmlSize) %>%
                         length(),
                     F, T)
    # Children
    l.Children <- if (isLeaf) {
        vector(mode="character",length=0)
    } else {
        sapply(XML::getNodeSet(MT, paste0(getChaine(listeNoeuds),
                                          "/ATTRIBUTE/NAME")),
               XML::xmlValue)
    }

    # Mother if any
    mother<-ifelse(length(listeNoeuds) > 1,
                   listeNoeuds[length(listeNoeuds) - 1],
                   character(0))

    # Sisters: they do have the same mother
    l.Sisters <- if (length(listeNoeuds)>1) {
        MT %>%
            XML::getNodeSet(paste0(getChaine(listeNoeuds[1:length(listeNoeuds)-1]),
                                   "/ATTRIBUTE/NAME")) %>%
            sapply(XML::xmlValue)
    } else {vector(mode = "character", length = 0)}
    l.Sisters <- l.Sisters[l.Sisters[] != listeNoeuds[length(listeNoeuds)]]

    # Scale and labels
    scaleNode <- MT %>%
        XML::getNodeSet(paste0(getChaine(listeNoeuds), "/SCALE/SCALEVALUE")) %>%
        sapply(XML::xmlSize) %>%
        length()

    scaleLabel <- MT %>%
        XML::getNodeSet(paste0(getChaine(listeNoeuds),
                               "/SCALE/SCALEVALUE/NAME")) %>%
        sapply(XML::xmlValue)

    # Aggregation function
    if (!isLeaf) {
        c.Function <- MT %>%
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
            scaleChildren[i] <- MT %>%
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
    } else if (MT %>%
               XML::getNodeSet(paste0(getChaine(listeNoeuds),
                                      "/FUNCTION/WEIGHTS")) %>%
               sapply(XML::xmlValue) %>%
               length()) {

        WeightList <- MT %>%
            XML::getNodeSet(paste0(getChaine(listeNoeuds),
                                   "/FUNCTION/WEIGHTS")) %>%
            sapply(XML::xmlValue) %>%
            strsplit(";") %>%
            unlist() %>%
            as.numeric()

    } else { WeightList <- -1 }

    # Output
    out <- new("Node",
               name = listeNoeuds[length(listeNoeuds)],
               Depth = length(listeNoeuds),
               isLeaf = isLeaf,
               mother = mother,
               sisters = l.Sisters,
               children = l.Children,
               aggregation = aggregation,
               rangeScale = scaleNode,
               scaleLabel = scaleLabel,
               Proba = WeightList,
               nodePath = listeNoeuds)
}


#' Title
#'
#' @param listNodes Node list
#' @param nodeName Node names
#'
#' @return
#' @export
#'
#' @examples
getID <- function(listNodes,nodeName) {
    out <- numeric(0)
    for(i in 1:length(listNodes)) {
        if(rev(listNodes[[i]]@nodePath)[1] == nodeName)
            out <- c(out, listNodes[[i]]@id)
    }
    return(out)
}


#' Title
#'
#' @param aTree A tree
#'
#' @return
#' @export
#'
#' @examples
EvaluateOrder <- function(aTree) {
    evalOrder <- numeric(0)
    if (aTree@isLeafAggregated) {

        results <- numeric(aTree@nbAttributes)
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
                       ifelse(results[aTree@Nodes[[x]]@name] == -1, 1, 0)
                   }) %>%
                   unlist() %>%
                   sum()) {
                    skip <- c(skip,l.Nodes[i])
                } else {
                    results[l.Nodes[i]] <- 1
                    id <- getID(aTree@Nodes, l.Nodes[i])
                    if(length(id) > 1)
                        id <- unlist(sapply(id, function(x) {
                            if(!aTree@Nodes[[x]]@isLeaf) {x}
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


#' Title
#'
#' @param aTree A Tree
#' @param nodeID The Node ID
#'
#' @return
#' @export
#'
#' @examples
getLeaves <- function(aTree,nodeID) {
    if (is.character(nodeID)) {
        l.id <- getID(aTree@Nodes, nodeID)
        nodeID <- unlist(sapply(l.id, function(x) {
            if(!aTree@Nodes[[x]]@isLeaf)aTree@Nodes[[x]]@id
        }))
    }

    l.Nodes <- unlist(sapply(aTree@Nodes, function(x) {
        if(length(grep(paste(aTree@Nodes[[nodeID]]@nodePath, collapse = ""),
                       paste(x@nodePath, collapse = ""))))x@id
    }))

    return(unlist(sapply(l.Nodes, function(x) {
        if (aTree@Nodes[[x]]@isLeaf) {aTree@Nodes[[x]]@id}
    })))
}
