#' Perform One-Factor-At-A-Time (OFAT) Sensitivity Analysis
#'
#' Conducts One-Factor-At-A-Time (OFAT) sensitivity analysis by performing
#' simulations with varying factors, keeping all but one factor constant for
#' each simulation.
#'
#' @param aTree A decision tree object to perform the analysis on.
#' @param option The initial configuration for the decision tree parameters.
#'
#' @return A matrix with evaluation results for each attribute in the decision
#'   tree, under different variations of the parameters.
#'
#' @export
OAT <- function(aTree,
                option) {

    # Define the matrix that will be returned:
    # nominal evaluation and for each leaves +1 and -1 and if touching border "-1"
    results <- matrix(nrow = aTree@NumberOfAttributes,
                      ncol = aTree@NumberOfLeaves*2 + 1)
    rownames(results) <- aTree@Attributes
    results[, 1] <- EvaluateScenario(aTree, as.matrix(option))
    j <- 2
    for(i in aTree@Leaves) {
        #Option +
        newOption <- option
        newOption[i, ] <- option[i, ] + 1
        if (newOption[i, ] > aTree@Nodes[[getID(aTree@Nodes, i)[1]]]@rangeScale) {
            results[, j]<- -1
        } else {
            results[, j] <- EvaluateScenario(aTree, as.matrix(newOption))
        }

        #Option -
        newOption <- option
        newOption[i, ] <- newOption[i, ] - 1
        if(newOption[i, ] == 0) {
            results[, j + 1] <- -1
        } else {
            results[, j + 1] <- EvaluateScenario(aTree, as.matrix(newOption))
        }

        j <- j + 2
    }

    return(results)
}



#' Visualize One-Factor-At-A-Time (OFAT) Sensitivity Analysis Results
#'
#' Generates a plot to visualize the results of One-Factor-At-A-Time (OFAT)
#' sensitivity analysis.
#'
#' @param nodeName Name of the node for which to generate the plot.
#' @param aResults Matrix of evaluation results returned by the OAT function.
#' @param aTree A decision tree object that was used in the OAT function.
#'
#' @return No return value, but generates a plot.
#'
#' @export
showOAT <- function(nodeName, aResults, aTree) {
    #On récupère l'ID du noeud
    id <- getID(aTree@Nodes, nodeName)
    # Si on est dans le cas d'une Leaf-Aggregated !
    #   on récupère le noeud aggrégé et non le noeud feuille
    if (aTree@IsLeafAggregated) {
        id <- id %>%
            sapply(function(x) {
                if (!aTree@Nodes[[x]]@isLeaf) {aTree@Nodes[[x]]@id}
            }) %>%
            unlist()
    }

    myChildren <- getLeaves(aTree, id)
    list1 <- aResults[nodeName, ]
    nominal <- rep(list1[1], length(myChildren))
    plus <- numeric(length(myChildren))
    minus <- numeric(length(myChildren))

    for(i in 1:length(myChildren)) {
        plus[i] <- list1[2*myChildren[i]]
        minus[i] <- list1[2*myChildren[i] + 1]
    }

    plot(1:length(myChildren), nominal,
         pch = "o", xlab = "", ylab = "Score", main = nodeName, axes = FALSE,
         ylim = c(1, aTree@Nodes[[id]]@rangeScale))
    points(1:length(myChildren), minus, pch = "-")
    points(1:length(myChildren), plus, pch = "+")
    axis(side = 1, at = 1:length(myChildren),
         labels = abbreviate(sapply(1:length(myChildren),
                                    function(x) {aTree@Nodes[[myChildren[x]]]@name})),
         las = 2)
    axis(side = 2, at = c(1:aTree@Nodes[[id]]@rangeScale),
         labels = 1:aTree@Nodes[[id]]@rangeScale)
}
