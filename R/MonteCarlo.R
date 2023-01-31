#' Title
#'
#' A simple function to give information on time requested to perform the MC
#'
#' @param aTree aTree
#' @param nbRuns nbRuns
#' @param iTest iTest
#'
#' @return
#' @export
#'
#' @examples
infoMC <- function(aTree,
                  nbRuns,
                  iTest = 50) {
    start_time <- Sys.time()
    option <- matrix(nrow = aTree@nbLeaves,
                     ncol = iTest)
    rownames(option) <- aTree@Leaves
    for(k in aTree@Leaves) {
        option[k, ] <- aTree@Nodes[[getID(aTree@Nodes,k)[1]]]@rangeScale %>%
            sample(size = iTest,
                   prob = aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@Proba,
                   replace = TRUE)
    }

    dummy <- sapply(1:iTest,
                    function(x) {EvaluateScenario(aTree,
                                                  as.matrix(option[, x]))})
    end_time <- Sys.time()
    cat("\n Approximative required time to run MC with ",
        nbRuns, " simulations",
        (end_time-start_time)*nbRuns/iTest/60, " minutes")
}



#' Title
#'
#' MonteCarlo approach
#'
#' @param aTree aTree
#' @param nbRuns nbRuns
#' @param isFile isFile
#' @param verbose verbose
#'
#' @return
#' @export
#'
#' @examples
MonteCarlo <- function(aTree,
                       nbRuns,
                       isFile = F,
                       verbose = T) {
    if (verbose) {
        cat("\n Time in: ", date())
    }

    MC <- matrix(ncol = nbRuns,
                 nrow = aTree@nbAttributes,
                 dimnames = list(c(aTree@Attributes), c(seq(1:nbRuns))))
    option <- matrix(nrow = aTree@nbLeaves,
                     ncol = nbRuns)
    rownames(option) <- aTree@Leaves

    for(k in aTree@Leaves) {
        option[k, ] <- aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@rangeScale %>%
            sample(size = nbRuns,
                   prob = aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@Proba,
                   replace = TRUE)
    }

    MC <- 1:nbRuns %>%
        sapply(function(x) {EvaluateScenario(aTree,
                                             as.matrix(option[, x]))})

    if (isFile) {
        # Write a file that contain all the random options selected for the MC analysis
        write.table(option,
                    file = "MC options.csv",
                    sep = ",", row.names = T, col.names = NA)
    }

    if (verbose) {
        cat("\n Time out: ",date())
    }
    return(MC)
}



#' Title
#'
#' @param Node Node
#' @param MC MC
#' @param nbRuns nbRuns
#'
#' @return
#' @export
#'
#' @importFrom graphics legend text
#'
#' @examples
ShowMC <- function(Node,
                 MC,
                 nbRuns) {

    #  Node<-theTree@Nodes[[nodeName]]
    typ <- "A"
    if (Node@isLeaf) { typ <- "L" }

    bar <- MC[Node@name, ] %>%
        tapply(MC[Node@name, ], sum)

    if (length(bar) < Node@rangeScale) {
        newbar <- array(data = 0,
                        dim = Node@rangeScale,
                        dimnames = list(c(seq(1:Node@rangeScale))))
        for(i in 1:dim(bar)) {
            newbar[names(bar)[i]] <- bar[i]
        }
        bar <- newbar
    }

    bar[] <- (bar[] / c(1:Node@rangeScale)) / nbRuns
    mc <- barplot(bar,
                  main = paste(Node@name, " [", typ, "]", sep = ""),
                  xlab = "Modalities",
                  ylab = "Frequencies",
                  ylim = c(0, (max(bar) + 0.1)),
                  las = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 0.9)
    #,names.arg=Node@scaleLabel

    text(mc, bar,
         format(round(bar, digits = 2)),
         xpd = T, cex = 0.8, pos = 3)

    #  ref_points<-as.matrix(read.table(file="MC bar lengths_ref.csv",sep=",",row.names=1))
    #  points(mc,as.vector(na.omit((ref_points))),pch=18,col=2)

    legend("topright",
           legend = paste(names(bar), abbreviate(Node@scaleLabel)),
           box.lty = 0, cex =0.8)
    text(length(bar))

    #write.table(bar,file="MC bar lengths.csv",sep=",",row.names=T,col.names=NA)
    cat(row.names = Node@name,
        bar,
        file = "MC bar lengths.csv",
        sep = ",", fill = T, append = T)
    return(bar)
}
