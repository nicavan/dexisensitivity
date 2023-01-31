#' Title
#'
#' Calculation of the Sensitivity Index (SI)
#'
#' @param aTree aTree
#' @param fileName fileName
#' @param isFile isFile
#'
#' @return
#' @export
#'
#' @examples
SI_DEXi <- function(aTree,
                    fileName = "SI_out.csv",
                    isFile = T) {
    SI <- vector(mode = "list",
                 length(aTree@Aggregated))
    names(SI) <- aTree@Aggregated

    for(node.name in aTree@Aggregated) {
        sousArbre <- createSubTree(aTree, node.name)
        l <- matrix(nrow = sousArbre@nbAttributes - 1,
                    ncol = 2)

        for(j in 2:sousArbre@nbAttributes) {
            l[j-1,1] <- sousArbre@Nodes[[j]]@isLeaf
            l[j-1,2] <- sousArbre@Nodes[[j]]@Depth - sousArbre@Nodes[[1]]@Depth + 1
        }

        zSI <- clcSI_DEXi(sousArbre)
        SI[[node.name]] <- matrix(c(zSI[c(sousArbre@Attributes[-1])], l),
                                  byrow = F,
                                  ncol = 3,
                                  dimnames =list(c(sousArbre@Attributes[-1]),
                                                 c("SI", "Leaf", "Depth")))
    }

    if (isFile) {
        for(i in 1:length(aTree@Aggregated)) {
            write.table(names(SI)[i],
                        file = fileName,
                        append = T,
                        sep = "",
                        row.names = F,
                        col.names = F)

            write.table(SI[[i]],
                        file = fileName,
                        append = T,
                        sep = ",",
                        row.names = T,
                        col.names = F)
        }
    }

    return(SI)
}


#' Title
#'
#' @param aTree aTree
#'
#' @return
#' @export
#'
#' @examples
clcSI_DEXi <- function(aTree) {
    nodeName <- aTree@rootName
    WeightList <- vector(mode = "list",
                         length = aTree@nbAttributes)
    names(WeightList) <- aTree@Attributes

    for(i in 1:aTree@nbAttributes) {
        WeightList[[i]] <- aTree@Nodes[[i]]@Proba
    }

    CondiProbaList <- vector(mode = "list",
                             length = aTree@nbAttributes)
    names(CondiProbaList) <-aTree@Attributes

    # Loop on the Aggregate attributes in reverse order
    for(node.name in rev(aTree@Aggregated)) {
        id <- getID(aTree@Nodes, node.name)
        if (length(id) > 1) {
            id <- id %>%
                sapply(function(x) {
                    if (!aTree@Nodes[[x]]@isLeaf) {x}
                }) %>%
                unlist()
        }

        Node <- aTree@Nodes[[id]]

        # info on weights required for direct descendant calculations
        ChildrenWeights <- vector(mode = "list",
                                  length = length(Node@children))
        for(i in seq(Node@children)) {
            ChildrenWeights[[i]] <- WeightList[[Node@children[[i]]]]
        }

        # conditional proba calculations (direct descendants)
        Probas <- condprob.direct(Node@aggregation,
                                  ChildrenWeights,
                                  Node@rangeScale)

        #In case of rangeScale <> of modalities in Y aggregation table, need to add some information in Probas
        if (Node@rangeScale != (length(Probas[[length(Node@children) + 1]]))) {
            nbC <- length(Node@children)
            nbVal <- length(Probas[[nbC + 1]])
            diff <- Node@rangeScale - nbVal
            Probas[[nbC + 1]][c((nbVal + 1):Node@rangeScale)] <- 0
            names(Probas[[nbC + 1]]) <- c(names(Probas[[nbC+1]][1:nbVal]),
                                          setdiff(as.character(seq(1:Node@rangeScale)),
                                                  names(Probas[[nbC + 1]])))
        }

        names(Probas) <- c(Node@children, node.name)
        NodeWeights <- Probas[[length(Probas)]]
        DirectCondiProbaList <- Probas[-length(Probas)]
        # conditional proba calculations (indirect descendants)
        IndirectCondiProbaList <- vector(mode = "list",
                                         length = 0)
        for(i in seq(Node@children)) {
            id  <- getID(aTree@Nodes,
                         Node@children[[i]])

            #If several nodes with the same name, we will choose the one with the proper mother
            if(length(id) > 1) {
                id <- id %>%
                    sapply(function(x) {
                        if (aTree@Nodes[[x]]@mother==Node@name) {x}
                    })
            }

            child.node <- aTree@Nodes[[id]]
            if (!child.node@isLeaf) {
                CPL.DA <- CondiProbaList[[Node@children[[i]]]]
                CPL.DY <- vector(mode = "list",
                                 length = length(CPL.DA))
                for(j in seq(CPL.DA)) {
                    CPL.DY[[j]] <- CPL.DA[[j]] %*% Probas[[Node@children[[i]]]]
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



#' Title
#'
#' Calculates the probabilities of Y=y and the probabilities of Y conditional
#' to its direct descendants A_i, when given the complete table of the
#' Y modalities with respect to the A_i factors, and when given the A_i
#' probabilities
#'
#' @param table a matrix giving all the level combinations of the A_i factors
#' and, in the last column, the associated Y values
#'
#' @param weightlist a list whose elements are the weight vectors of each A_i
#' variable. If missing, A_i levels are assumed to have equal weights
#'
#' @param sy sy
#'
#' @param Ylevels optional argument giving the Y levels. If missing, the Ylevels
#'  are extracted from the table
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param condproblist condproblist
#' @param weightlist weightlist
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @param aTree aTree
#' @param aSI aSI
#'
#' @return
#' @export
#'
#' @importFrom graphics mtext
#'
#' @examples
showSI <- function(aTree,
                   aSI) {
    #   par(mgp=c(7,1,0),oma=c(0,20,0,0))
    mc <- barplot(as.vector(rev(aSI[aTree@Leaves,1])),
                  horiz = T,
                  xlim = c(0, max(aSI[aTree@Leaves,1])),
                  ylab = "Indicators")
    axis(side = 2,
         at = mc,
         labels = rev(aTree@Leaves),
         las = 2)
    abline(v = 0.02, untf = FALSE, lty = 3)
    mtext("Sensitity Index", 1, line = 3)
    mtext("Basic attributes", 2, outer = T, line = 15)
}
