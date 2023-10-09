#' Calculate Sensitivity Index (SI)
#'
#' Calculates the Sensitivity Index (SI) for a given decision tree.
#'
#' @param tree A decision tree object to perform the analysis on.
#' @param file_name The file name to write the SI to. Default is "SI_out.csv".
#' @param is_file A boolean to decide whether to write the SI to a file. Default
#'   is TRUE.
#' @param avoid_repetition A boolean to decide whether to avoid repeated nodes.
#'   Default is FALSE.
#'
#' @return A list of Sensitivity Indices for each node in the tree.
#'
#' @export
si_dexi <- function(tree,
                    file_name = "SI_out.csv",
                    is_file = TRUE,
                    avoid_repetition = FALSE) {

  # Initialize SI list for each aggregated node in the decision tree
  si <- vector(mode = "list", length = length(tree@Aggregated))
  names(si) <- tree@Aggregated

  # Calculate SI for each aggregated node
  for (node_name in tree@Aggregated) {
    # Generate a subtree based on the current node and avoidance settings
    sub_tree <- create_sub_tree(tree, node_name,
                                avoid_repetition = avoid_repetition)

    # Prepare a matrix to store leaf status and depth information for each
    # attribute
    leaf_depth_info <- matrix(nrow = sub_tree@NumberOfAttributes - 1, ncol = 2)

    # Populate the leaf status and depth information
    for (j in 2:sub_tree@NumberOfAttributes) {
      leaf_depth_info[j-1, 1] <- sub_tree@Nodes[[j]]@IsLeaf
      leaf_depth_info[j-1, 2] <- sub_tree@Nodes[[j]]@Depth - sub_tree@Nodes[[1]]@Depth + 1
    }

    # Calculate the Sensitivity Index for the subtree using the external
    # function `clcSI_DEXi`
    calculated_si <- clcSI_DEXi(sub_tree, avoid_repetition = avoid_repetition)

    # Merge Sensitivity Index results with leaf and depth data, then store in
    # the main SI list
    si[[node_name]] <- matrix(c(calculated_si[c(sub_tree@Attributes[-1])],
                                leaf_depth_info),
                              byrow = FALSE,
                              ncol = 3,
                              dimnames = list(c(sub_tree@Attributes[-1]),
                                              c("SI", "Leaf", "Depth")))
  }

  # Write the SI values to a file if required
  if (is_file) {
    for (i in 1:length(tree@Aggregated)) {
      # First, write the node name header
      write.table(names(si)[i],
                  file = file_name,
                  append = TRUE,
                  sep = "",
                  row.names = FALSE,
                  col.names = FALSE)

      # Next, append the corresponding SI values for that node
      write.table(si[[i]],
                  file = file_name,
                  append = TRUE,
                  sep = ",",
                  row.names = TRUE,
                  col.names = FALSE)
    }
  }

  return(si)
}


#' Calculate Sensitivity Index (SI) Helper Function
#'
#' A helper function for `SI_DEXi` to calculate Sensitivity Index (SI) of a
#' decision tree.
#'
#' @param tree A decision tree object to perform the analysis on.
#' @param avoid_repetition A boolean to decide whether to avoid repeated nodes. Default
#'   is FALSE.
#'
#' @return A vector of Sensitivity Indices for each attribute in the decision
#'   tree.
#'
#' @export
clcSI_DEXi <- function(tree, avoid_repetition = F) {
    nodeName <- tree@RootName
    WeightList <- vector(mode = "list",
                         length = tree@NumberOfAttributes)
    names(WeightList) <- tree@Attributes

    for(i in 1:tree@NumberOfAttributes) {
        WeightList[[i]] <- tree@Nodes[[i]]@Probability
    }

    CondiProbaList <- vector(mode = "list",
                             length = tree@NumberOfAttributes)
    names(CondiProbaList) <-tree@Attributes

    depthorder <- depth_order(tree)

    # Loop on the Aggregate attributes in reverse order
    for(node.name in depthorder) {
        id <- get_id(tree@Nodes, node.name)
        if (length(id) > 1) {
            id <- id %>%
                sapply(function(x) {
                    if (!tree@Nodes[[x]]@IsLeaf) {x}
                }) %>%
                unlist()
        }

        # to avoid repeted branch
        if (length(id)>1 & avoid_repetition) {
            id <- id[1]
        }

        Node <- tree@Nodes[[id]]

        # info on weights required for direct descendant calculations
        ChildrenWeights <- vector(mode = "list",
                                  length = length(Node@Children))
        for(i in seq(Node@Children)) {
            ChildrenWeights[[i]] <- WeightList[[Node@Children[[i]]]]
        }

        # conditional proba calculations (direct descendants)
        Probas <- condprob.direct(Node@Aggregation,
                                  ChildrenWeights,
                                  Node@RangeScale)

        #In case of rangeScale <> of modalities in Y aggregation table, need to add some information in Probas
        if (Node@RangeScale != (length(Probas[[length(Node@Children) + 1]]))) {
            nbC <- length(Node@Children)
            nbVal <- length(Probas[[nbC + 1]])
            diff <- Node@RangeScale - nbVal
            Probas[[nbC + 1]][c((nbVal + 1):Node@RangeScale)] <- 0
            names(Probas[[nbC + 1]]) <- c(names(Probas[[nbC+1]][1:nbVal]),
                                          setdiff(as.character(seq(1:Node@RangeScale)),
                                                  names(Probas[[nbC + 1]])))
        }

        names(Probas) <- c(Node@Children, node.name)
        NodeWeights <- Probas[[length(Probas)]]
        DirectCondiProbaList <- Probas[-length(Probas)]
        # conditional proba calculations (indirect descendants)
        IndirectCondiProbaList <- vector(mode = "list",
                                         length = 0)
        for(i in seq(Node@Children)) {
            id  <- get_id(tree@Nodes,
                         Node@Children[[i]])

            #If several nodes with the same name, we will choose the one with the proper mother
            if(length(id) > 1) {
                id <- id %>%
                    sapply(function(x) {
                        if (tree@Nodes[[x]]@Mother==Node@Name) {x}
                    }) %>%
                    unlist()
            }

            # to avoid repeted branch
            if (length(id)>1 & avoid_repetition) {
                id <- id[1]
            }

            child.node <- tree@Nodes[[id]]
            if (!child.node@IsLeaf) {
                CPL.DA <- CondiProbaList[[Node@Children[[i]]]]
                CPL.DY <- vector(mode = "list",
                                 length = length(CPL.DA))
                for(j in seq(CPL.DA)) {
                    CPL.DY[[j]] <- CPL.DA[[j]] %*% Probas[[Node@Children[[i]]]]
                }
                names(CPL.DY) <- names(CPL.DA)
                IndirectCondiProbaList <- c(IndirectCondiProbaList, CPL.DY)
            }
        }
        WeightList[[node.name]] <- NodeWeights
        CondiProbaList[[node.name]] <- c(DirectCondiProbaList,
                                         IndirectCondiProbaList)
    }

    CPL <- c(CondiProbaList[[nodeName]], WeightList[nodeName])
    Wgt <- WeightList[names(CondiProbaList[[nodeName]])]
    SI <- sensitivity.condprob(condproblist = CPL,
                               weightlist = Wgt)

    return(SI)
}


#' Calculate Conditional Probabilities
#'
#' Calculates the probabilities of Y=y and the probabilities of Y conditional to
#' its direct descendants A_i.
#'
#' @param table A matrix giving all the level combinations of the A_i factors
#'   and, in the last column, the associated Y values.
#' @param weightlist A list whose elements are the weight vectors of each A_i
#'   variable. If missing, A_i levels are assumed to have equal weights.
#' @param sy The number of unique Y values.
#' @param Ylevels Optional argument giving the Y levels. If missing, the Ylevels
#'   are extracted from the table.
#'
#' @return A list containing the matrices of Y probabilities conditional to each
#'   A_i factor and, in the last position, the vector of marginal Y
#'   probabilities.
#'
#' @export
condprob.direct <- function(table,
                            weightlist,
                            sy,
                            Ylevels) {
    # Calculates the probabilities of Y=y and the probabilities of Y conditional
    # to its direct descendants A_i, when given the complete table of the
    # Y modalities with respect to the A_i factors, and when given the A_i
    # probabilities
    # ARGUMENTS
    #  table : a matrix giving all the level combinations of the A_i
    #          factors and, in the last column, the associated Y values
    #  weightlist : a list whose elements are the weight vectors of each A_i
    #               variable
    #               if missing, A_i levels are assumed to have equal weights
    #  Ylevels : optional argument giving the Y levels. If missing, the Ylevels
    #            are extracted from the table
    # OUTPUT
    #  a list containing the matrices of Y probabilities conditional to each A_i
    #  factor and, in the last position, the vector of marginal Y probabilities
    # DETAILS
    #  The Ylevels argument is useful to cope with the cases when not all
    #  Ylevels are in the table
    # EXAMPLE
    #  toto <- condprob.direct(ImpMilTable, weightlist)

    # Preliminary calculations
    A <- table[, -ncol(table), drop = F]
    Y <- table[, ncol(table)]
    # - number of factors A (n)
    #   and numbers of levels of the factors A (s) and of Y (sY)
    n <- ncol(A)
    s <- apply(A, 2, function(x) {length(unique(x))})

    if (missing(Ylevels)) {
        Ylevels <- sort(unique(Y))
    }

    # equal weights if missing
    if (missing(weightlist)) {
        weightlist <- lapply(s, function(n){rep(1,n)/n})
    }

    # Weights of the table rows for each A variable and for Y
    Aweights <- matrix(NA, nrow(A), n)
    for (i in 1:n) {
        Aweights[,i] <- weightlist[[i]][A[, i]]
    }

    Yweights <- apply(Aweights, 1, prod)

    # Calculation of the Y probabilities
    Yproba <- c( tapply(Yweights, Y, sum) )

    # Calculation of the Y probabilities conditional to the A_i
    YAproba <- vector("list", length = n)

    for(i in 1:n) {
        probas.i <- Yweights/Aweights[, i]
        condproba <-  tapply(probas.i, list(A[, i], Y), sum)
        condproba[is.na(condproba)] <- 0
        YAproba[[i]] <- condproba
    }

    # Results storage
    out <- c(YAproba, list(Yproba))
    names(out) <- colnames(table)

    #Modification in case sy>unique(Y)
    if (sy > length(unique(Y))) {
        newlist <- vector(mode = "list",
                          length = n+1)
        for(j in 1:n) {
            newlist[[j]] <- matrix(0, nrow = dim(out[[j]])[1], ncol = sy)
            dimnames(newlist[[j]])[1] <- c(dimnames(out[[j]])[1])
            dimnames(newlist[[j]])[2] <- list(c(1:sy))
            for(k in as.numeric(dimnames(out[[j]])[[2]])) {
                newlist[[j]][, k] <- out[[j]][, as.character(k)]
            }
        }

        newlist[[n + 1]] <- rep(0, sy)
        names(newlist[[n+1]]) <- c(1:sy)

        for(k in as.numeric(names(Yproba))) {
            newlist[[n + 1]][k] <- Yproba[as.character(k)]
        }

        names(newlist) <- colnames(table)
        return(newlist)
    }

    return(out)
}

#' Calculate Sensitivity Index (SI) From Conditional Probabilities
#'
#' Calculates the first order sensitivity indices of Y with respect to A_i
#' descendants, when given the conditional probabilities and A_i weights.
#'
#' @param condproblist A list of matrices of conditional probabilities (Y
#'   conditional to each A_i) plus the vector of Y probabilities.
#' @param weightlist The list of weights of the A_i factor levels.
#'
#' @return A vector of Sensitivity Indices (SI).
#'
#' @export
sensitivity.condprob <- function(condproblist,
                                 weightlist) {
    # Calculates the first order sensitivity indices of Y with respect
    # to A_i descendants, when given the conditional probabilities and A_i weights
    # ARGUMENTS
    #  condproblist : list of matrices of conditional probabilities
    #             (Y conditional to each A_i) + the vector of Y probabilities
    #  weightlist : the list of weights of the A_i factor levels
    # OUTPUT
    #  a vector of SI sensitivity indices
    # EXAMPLE
    #
    #

    # preliminaries
    n <- length(condproblist) - 1
    Yproba <- condproblist[[n + 1]]
    Ylevels <- as.numeric( names(Yproba) )
    # Y expectation and variance
    Yexp <- sum( Yproba*Ylevels )
    Yvar <- sum( Yproba*(Ylevels-Yexp)^2 )
    # sensitivity indices
    SI <- vector(length = n)
    for(i in seq(n)) {
        Ycondexp.Ai <- condproblist[[i]] %*% Ylevels
        Yvar.Ai <- sum( weightlist[[i]]*(Ycondexp.Ai-Yexp)^2 )
        SI[i] <- Yvar.Ai/Yvar
    }

    # Results storage
    names(SI) <- names(condproblist[seq(n)])
    return(SI)
}


#' Show Sensitivity Index (SI)
#'
#' Generates a bar plot to visualize the Sensitivity Index (SI) of the leaves of
#' a given decision tree.
#'
#' @param tree A decision tree object to perform the analysis on.
#' @param aSI A vector of Sensitivity Indices for each leaf in the decision
#'   tree.
#'
#' @return No return value, but generates a bar plot.
#'
#' @importFrom graphics mtext
#'
#' @export
showSI <- function(tree,
                   aSI) {
    #   par(mgp=c(7,1,0),oma=c(0,20,0,0))
    mc <- barplot(as.vector(rev(aSI[tree@Leaves,1])),
                  horiz = T,
                  xlim = c(0, max(aSI[tree@Leaves,1])),
                  ylab = "Indicators")
    axis(side = 2,
         at = mc,
         labels = rev(tree@Leaves),
         las = 2)
    abline(v = 0.02, untf = FALSE, lty = 3)
    mtext("Sensitity Index", 1, line = 3)
    mtext("Basic attributes", 2, outer = T, line = 15)
}
