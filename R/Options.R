#' Generate a matrix of random options for a given tree
#'
#' The options are generated based on the `rangeScale` and `Proba` properties of
#' the tree nodes. This is useful for creating random scenarios for simulation
#' or analysis.
#'
#' @param aTree An object of class Tree.
#' @param nbOptions n integer representing the number of options to generate.
#'   Default is 1.
#' @param aSeed An integer seed to be used for random number generation. Default
#'   is NULL, which means no seed is set.
#'
#' @return A matrix of size `aTree@NumberOfLeaves` x `nbOptions`, where each row
#'   represents a leaf of the tree and each column represents a sampled option.
#'
#' @export
createOptions <- function(aTree, nbOptions = 1, aSeed = NULL) {
    if (!is.null(aSeed)) {
        set.seed(aSeed)
    }

    option <- matrix(nrow = aTree@NumberOfLeaves, ncol = nbOptions)
    rownames(option) <- aTree@Leaves

    for(k in aTree@Leaves) {
        option[k, ] <- sample(aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@rangeScale,
                              size = nbOptions,
                              prob = aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@Proba,
                              replace = TRUE)
    }

    return(option)
}


#' Evaluates a single scenario for a given tree
#'
#' Evaluates a single scenario by assigning values to the leaves of the tree
#' according to the given option and calculates the values of the aggregated
#' nodes.
#'
#' @param aTree A "Tree" object
#' @param option A matrix representing a single option to evaluate
#'
#' @return A numeric vector representing the evaluation results of the scenario
#'
#' @export
EvaluateScenario <- function(aTree, option) {

    # Create the return array
    if (!is.matrix(option)) {
        stop(cat("\nl'option n'est pas une matrice"), call)
    }

    results <- numeric(aTree@NumberOfAttributes)
    names(results) <- aTree@Attributes
    results[] <- -1

    # Give value for all leaves --- Deal with multiple leaves.
    # Leaf-Aggretated leaves should not be in option
    for(i in 1:length(option)) {
        results[which(names(results) == dimnames(option)[[1]][i])] <- option[i]
    }

    # If leaf aggregated leaves, deal first with the subtree
    if (aTree@IsLeafAggregated) {
        for(i in 1:length(aTree@EvaluationOrder)) {
            subTree <- createSubTree(aTree,
                                     aTree@Attributes[aTree@EvaluationOrder[i]])

            #Get the proper option
            subOption <- as.matrix(results[c(subTree@Leaves)])
            for(j in rev(subTree@Aggregated)) {

                # Compute the value thanks to the attribution table
                if(results[j] < 0) {
                    id <- getID(subTree@Nodes, j)

                    if(length(id) > 1) {
                        id <- id %>%
                            sapply(function(x) {
                                if (!subTree@Nodes[[x]]@isLeaf) {x}
                            }) %>%
                            unlist()
                    }

                    # It may happen that the tree structure is repeated !!!!
                    for(ii in id) {
                        nbc <- length(subTree@Nodes[[ii]]@children)
                        value <- subTree@Nodes[[ii]]@aggregation

                        for(k in 1:nbc) {
                            value <- value[value[, k] == results[subTree@Nodes[[ii]]@children[k]], ]
                        }

                        results[j]<-value[nbc+1]
                    }
                }
            }
        }
    }

    # Thanks to the hierchical structure of the tree... use rev()
    for(k in aTree@Attributes) {
        results[getID(aTree@Nodes, k)] <- max(results[getID(aTree@Nodes, k)])
    }

    for(i in rev(aTree@Aggregated)) {
        #Compute the value thanks to the attribution table
        if (results[i] < 0) {
            id <- getID(aTree@Nodes, i)

            if(length(id) > 1) {
                id <- id %>%
                    sapply(function(x) {if (!aTree@Nodes[[x]]@isLeaf) {x}})
            }

            nbc <- length(aTree@Nodes[[id]]@children)
            value <- aTree@Nodes[[id]]@aggregation

            for(j in 1:nbc) {
                value <- value[value[, j] == results[aTree@Nodes[[id]]@children[j]], ]
            }

            results[i] <- value[nbc + 1]
        }
    }

    for(k in aTree@Attributes) {
        results[getID(aTree@Nodes, k)] <- max(results[getID(aTree@Nodes, k)])
    }

    return(results)
}

#' Evaluate multiple scenarios for a given tree
#'
#' Works as a wrapper for the `EvaluateScenario` function that allows evaluation
#' of multiple scenarios at once. The scenarios are given as columns in the
#' `options` matrix.
#'
#' @param aTree A "Tree" object
#' @param options A matrix representing multiple options to evaluate
#'
#' @return A list of numeric vectors representing the evaluation results of the
#'   scenarios
#'
#' @export
EvaluateScenarios <- function(aTree, options) {
    1:dim(options)[2] %>%
        sapply(function(x) {
            EvaluateScenario(aTree, as.matrix(options[, x]))
        })
}


#' Save a table of options to a file
#'
#' Stores a table of options into a file for later use or analysis.
#'
#' @param anOptionsTable A matrix representing options
#' @param aFileName The name of the file to save the options table
#'
#' @return NULL
#'
#' @export
saveOptions <- function(anOptionsTable,
                        aFileName) {
    utils::write.table(anOptionsTable,
                       file = aFileName,
                       sep = "\t",
                       row.names = T,
                       col.names = NA,
                       quote = FALSE)
}



#' Load a table of options from a file
#'
#' Retrieves previously saved options from a file for further analysis or
#' processing.
#'
#' @param filename The name of the file to load the options table from
#'
#' @return A matrix representing the loaded options table
#'
#' @export
loadOptions <- function(filename) {
    return(as.matrix(utils::read.table(file = filename,
                                       header = T,
                                       sep = "\t",
                                       row.names = 1)))
}


#' Save evaluation results of scenarios to a file
#'
#' Stores the results of scenario evaluations into a file for later analysis.
#'
#' @param theScenarios A list of numeric vectors representing the evaluation
#'   results of the scenarios
#' @param file The name of the file to save the scenarios
#'
#' @return NULL
#'
#' @export
saveScenarios <- function(theScenarios,
                          file) {
    utils::write.table(theScenarios,
                       file = file,
                       sep = "\t",
                       row.names = T,
                       col.names = NA)
}



#' Plot a bar chart of a single scenario
#'
#' Visualizes the values assigned to each attribute in a given scenario and
#' marks the maximum possible value for each attribute using a bar chart.
#'
#' @param aScenario the scenario to graph
#' @param aTree The associated "Tree" object
#' @param isLabelY A logical value indicating whether to include labels on the Y
#'   axis (Default: TRUE)
#' @param isPar A logical value indicating whether to modify the graph's
#'   parameters (Default: TRUE)
#'
#' @return NULL
#'
#' @seealso \code{\link{EvaluateScenario}}
#'
#' @importFrom withr defer
#'
#' @export
showScenario <- function(aScenario,
                         aTree,
                         isLabelY = TRUE,
                         isPar = T) {
    if (isPar) {
        oldpar <- par(mgp = c(7,1,0), oma = c(0,20,0,0), cex = 0.5)
        withr::defer(par(oldpar))
    }

    # Determine the gray scale, we use grey.scale
    myGreyValue <- lapply(1:7, function(x) {grDevices::gray.colors(x, 0, 1) })
    myCol <- aTree@Leaves %>%
        sapply(function(x) {
            myGreyValue[[aTree@Nodes[[getID(aTree@Nodes, x)[1]]]@rangeScale]][aScenario[x, ]]
        }) %>%
        unlist()

    theMax <- aTree@Attributes %>%
        lapply(function(x) {
            aTree@Nodes[[getID(aTree@Nodes, x)[1]]]@rangeScale
        }) %>%
        unlist() %>%
        matrix(ncol = 1)

    mc <- graphics::barplot(as.vector(rev(aScenario)),
                            xlim = c(0, max(theMax[]) + 0.5),
                            ylab = "Indicators",
                            xlab = "Mark",
                            horiz = T,
                            col = rev(myCol))

    if (isLabelY) {
        graphics::axis(side = 2,
                       at = mc,
                       labels = rev(rownames(aScenario)),
                       las = 2,
                       cex = 0.5)
    }

    graphics::points(as.vector(rev(theMax)),
                     mc,
                     col = "black",
                     pch = "<")

    graphics::abline(v = c(1:max(theMax)), untf = FALSE, lty = 3)
}


#' Compare scenarios using a radial plot
#'
#' Visualizes the comparison of values assigned to a list of nodes across
#' multiple scenarios using a radial plot.
#'
#' @param aTree A "Tree" object
#' @param theScenarios A list of numeric vectors representing the evaluation
#'   results of the scenarios
#' @param listNodes A list of node names to include in the comparison
#'
#' @seealso \code{\link{EvaluateScenarios}}
#'
#' @return NULL
#' @export
#'
#' @importFrom plotrix radial.plot
compareScenario <- function(aTree,
                            theScenarios,
                            listNodes) {
    oldpar <- par(ps = 6)
    withr::defer(par(oldpar))

    plotrix::radial.plot(t(theScenarios[listNodes, ]),
                         labels = abbreviate(names.arg = listNodes,
                                             minlength = 6),
                         rp.type = "p",
                         start = pi/2,
                         main = "Comparison of scenarios",
                         line.col = "blue",
                         lwd = 3)
}
