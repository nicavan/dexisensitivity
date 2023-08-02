#' Estimates the execution time for factorial simulations
#'
#' Performs a time estimation for a specified number of factorial simulations.
#' The estimation is based on the time taken to execute a smaller number of
#' simulations specified by `iTest`.
#'
#' @param aTree Decision tree to run simulations on.
#' @param iTest Number of simulations to be used for time estimation, defaults
#'   to 50.
#'
#' @return No explicit return. Prints out the estimated execution time.
#'
#' @export
infoAOV <- function(aTree, iTest = 50) {
    # Compute the size of the factorial plan and the time requested to run it
    preprod <- aTree@Nodes %>%
        sapply(function(x) {if (x@IsLeaf) {x@RangeScale}}) %>%
        unlist() %>%
        cumprod()
    nbRuns <- preprod[aTree@NumberOfLeaves]

    start_time <- Sys.time()
    option <- matrix(nrow = aTree@NumberOfLeaves,
                     ncol = iTest)
    rownames(option) <- aTree@Leaves
    for(k in aTree@Leaves) {
        option[k,] <- sample(aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@RangeScale,
                             size = iTest,
                             prob = aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@Probability,
                             replace = TRUE)
    }

    dummy <- sapply(1:iTest,
                    function(x) {
                        EvaluateScenario(aTree, as.matrix(option[, x]))
                    })
    end_time <- Sys.time()
    cat("\n", aTree@NumberOfLeaves, " factors",
        "\n Approximative required time to run the ", nbRuns, " modalities",
        (end_time-start_time)*nbRuns/iTest/60, " minutes")
}


#' Calculate sensitivity criteria for model terms
#'
#' Calculates sensitivity criteria for each term in a fitted model, including
#' degree of freedom, sum of squares, ratio of sum of squares to total sum of
#' squares, mean squares, and the F value.
#'
#' @param aov.obj An object of class `aov` resulting from a call to `aov()`.
#'
#' @return A data frame with the degree of freedom, sum of squares, ratio of sum
#'   of squares to total sum of squares, mean squares, and the F value for each
#'   term in the model. The rows of the data frame are ordered in decreasing
#'   order of the ratio of sum of squares to total sum of squares.
#'
#' @export
sensib.effet <- function(aov.obj) {
    # PRELIMINARIES: ANOVA results retrieval
    indic.fact <- attr(aov.obj$terms, "factors")[-1, ]
    aov.summ <- summary(aov.obj)[[1]]
    aov.ss <- aov.summ[, "Sum Sq"]
    aov.df <- aov.summ[, "Df"]
    aov.cm <- aov.ss/aov.df

    # Total SS, df and MS
    tss <- sum(aov.ss)
    tdf <- sum(aov.df)
    tms <- tss/tdf

    # Residual SS, df, MS
    rdf <- aov.obj$df.residual
    if (rdf > 0) {
        rss <- aov.ss[length(aov.ss)]
        rms <- rss/rdf
    } else {
        rss <- NA
        rms <- NA
    }

    # Sensitivity criteria term by term
    out <- data.frame(df = aov.df, ss = aov.ss, ss.ratio = aov.ss/tss,
                      cm = aov.cm, F = aov.cm/rms)
    rownames(out) <- rownames(aov.summ)
    out <- out[rev(order(out$ss.ratio)), ]

    return(out)
}
# sensib.effet <- function(aov.obj) {
#
#     #### PRELIMINAIRES: récupération des résultats d'anova
#     # indic.fact: matrice 0-1 de correspondance facteurs*termes-du-modèle
#     indic.fact <- attr(aov.obj$terms, "factors")[-1, ]
#     # aov.df: vecteur des Degrés de Liberté, résiduelle comprise
#     # aov.ss: vecteur des Sommes de Carrés, résiduelle comprise
#     # aov.cm: vecteur des Carrés Moyens, résiduelle comprise
#     ### ATTENTION ###
#     # sous S:
#     #aov.summ <- summary(aov.obj)
#     #aov.ss <- aov.summ[,"Sum of Sq"]
#     # sous R:
#     aov.summ <- summary(aov.obj)[[1]]
#     aov.ss <- aov.summ[, "Sum Sq"]
#     ### FIN DE "ATTENTION" ###
#     aov.df <- aov.summ[, "Df"]
#     aov.cm <- aov.ss/aov.df
#
#     # Total SS, df and MS
#     tss <- sum(aov.ss)
#     tdf <- sum(aov.df)
#     tms <- tss/tdf
#     # Residual SS, df, MS
#     rdf <- aov.obj$df.residual
#     if (rdf>0) {
#         rss <- aov.ss[length(aov.ss)]
#         rms <- rss/rdf
#     } else {
#         rss <- NA
#         rms <- NA
#     }
#
#     #-------------------------------------------------------------------------
#     #### Critères de sensibilité terme par terme
#     #-------------------------------------------------------------------------
#     # sorties
#     out <- data.frame(df = aov.df, ss = aov.ss, ss.ratio = aov.ss/tss,
#                       cm = aov.cm, F = aov.cm/rms)
#     rownames(out) <- rownames(aov.summ)
#     out <- out[rev(order(out$ss.ratio)), ]
# }


#' Calculate sensitivity factors for model terms
#'
#' Calculates sensitivity factors for each term in a fitted model based on
#' specific factors of interest. The function uses the parameters `las` and
#' `...` to control the appearance of the generated bar plot.
#'
#'
#' @param aov.obj An object of class `aov` resulting from a call to `aov()`.
#' @param fact.interet Factors of interest.
#' @param las The style of axis labels on the bar plot. Default is 1.
#' @param ... Additional arguments to control the appearance of the bar plot.
#'
#' @return A data frame with sensitivity factors for each term in the model.
#'
#' @export
sensib.grpe <- function(aov.obj,
                        fact.interet,
                        las = 1,
                        ...) {

    #### PRELIMINAIRES: récupération des résultats d'anova
    # indic.fact: matrice 0-1 de correspondance facteurs*termes-du-modèle
    indic.fact <- attr(aov.obj$terms, "factors")[-1, ]
    # aov.df: vecteur des Degrés de Liberté, résiduelle comprise
    # aov.ss: vecteur des Sommes de Carrés, résiduelle comprise
    # aov.cm: vecteur des Carrés Moyens, résiduelle comprise
    #### ATTENTION ####
    # sous S:
    #aov.summ <- summary(aov.obj)
    #aov.ss <- aov.summ[,"Sum of Sq"]
    # sous R:
    aov.summ <- summary(aov.obj)[[1]]
    aov.ss <- aov.summ[, "Sum Sq"]
    #### FIN DE "ATTENTION" ####
    aov.df <- aov.summ[, "Df"]
    aov.cm <- aov.ss/aov.df

    # Total SS, df and MS
    tss <- sum(aov.ss)
    tdf <- sum(aov.df)
    tms <- tss/tdf
    # Residual SS, df, MS
    rdf <- aov.obj$df.residual
    if (rdf > 0) {
        rss <- aov.ss[length(aov.ss)]
        rms <- rss/rdf
    } else {
        rss <- NA
        rms <- NA
    }


    #### Critères de sensibilité par groupes d'intérêt

    # calculs
    if (rdf > 0) {
        aov.ss <- aov.ss[-length(aov.ss)]
        aov.df <- aov.df[-length(aov.df)]
    }
    # Décomposition entre effets d'intérêt et autres
    facteurs <- rownames(indic.fact)
    fact.interet <- apply(outer(facteurs,
                                fact.interet,
                                "=="),
                          1, any)
    if (sum(fact.interet) == 1) {
        filtre1 <- indic.fact[fact.interet, ]
    } else {
        filtre1 <- apply(indic.fact[fact.interet, ], 2, any)
    }

    if (sum(!fact.interet) == 1) {
        filtre2 <- indic.fact[!fact.interet, ]
    } else {
        filtre2 <- apply(indic.fact[!fact.interet, ], 2, any)
    }
    term1 <- filtre1 & (!filtre2)
    term2 <- (!filtre1) & filtre2
    term3 <- filtre1 & filtre2
    indic.intrt <- rbind(term1, term2, term3)
    intrt.ss <- indic.intrt %*% aov.ss
    intrt.df <- indic.intrt %*% aov.df
    intrt.cm <- intrt.ss/intrt.df
    # sorties
    out <- data.frame(df = intrt.df, ss = intrt.ss, ss.ratio = intrt.ss/tss,
                      cm = intrt.cm, F = intrt.cm/rms)
    rownames(out) <- c("F.int.", "Autres F.", "Interaction")
    out <- out[rev(order(out$ss.ratio)), ]

    #### Graphiques

    outgraph <- out[rev(1:nrow(out)), ]
    outnames <- rownames(outgraph)
    outnames <- gsub("([ ])", "", outnames)
    barplot(outgraph$ss.ratio,
            horiz = T,
            names.arg = outnames,
            main = "Sums of Squares proportions",
            las = las,
            ...)

    out

}


#' Calculate sensitivity factors for model terms
#'
#' Calculates sensitivity factors for each term in a fitted model.
#'
#' @param aov.obj An object of class `aov` resulting from a call to `aov()`.
#'
#' @return A data frame with sensitivity factors for each term in the model.
#'
#' @export
sensib.total <- function(aov.obj) {

    #### PRELIMINAIRES: récupération des résultats d'anova

    # indic.fact: matrice 0-1 de correspondance facteurs*termes-du-modèle
    indic.fact <- attr(aov.obj$terms, "factors")[-1, ]
    # aov.df: vecteur des Degrés de Liberté, résiduelle comprise
    # aov.ss: vecteur des Sommes de Carrés, résiduelle comprise
    # aov.cm: vecteur des Carrés Moyens, résiduelle comprise
    #### ATTENTION ####
    # sous S:
    #aov.summ <- summary(aov.obj)
    #aov.ss <- aov.summ[,"Sum of Sq"]
    # sous R:
    aov.summ <- summary(aov.obj)[[1]]
    aov.ss <- aov.summ[, "Sum Sq"]
    #### FIN DE "ATTENTION" ####
    aov.df <- aov.summ[, "Df"]
    aov.cm <- aov.ss/aov.df

    # Total SS, df and MS
    tss <- sum(aov.ss)
    tdf <- sum(aov.df)
    tms <- tss/tdf
    # Residual SS, df, MS
    rdf <- aov.obj$df.residual
    if (rdf>0) {
        rss <- aov.ss[length(aov.ss)]
        rms <- rss/rdf
    } else {
        rss <- NA
        rms <- NA
    }


    #### Critères de sensibilité totale

    # calculs
    if (rdf>0) {
        aov.ss <- aov.ss[-length(aov.ss)]
        aov.df <- aov.df[-length(aov.df)]
    }
    total.ss <- indic.fact %*% aov.ss
    total.df <- indic.fact %*% aov.df
    total.cm <- total.ss/total.df
    # calculs effets principaux
    filtre.main <- apply(indic.fact, 2, sum) == 1
    main.ss <- indic.fact[, filtre.main] %*% aov.ss[filtre.main]
    # sorties
    out <- data.frame(df = total.df, ss = total.ss, ss.ratio = total.ss/tss,
                      main.ss.ratio = main.ss/tss, cm = total.cm,
                      F = total.cm/rms)
    out <- out[rev(order(out$ss.ratio)), ]
}


#' Visualize AOV results
#'
#' Visualizes the results of an Analysis of Variance (AOV).
#'
#' @param aAOV_DEXi A list containing the AOV results.
#' @param main.show Logical, if TRUE, main effects and total sums of squares are
#'   displayed in the plot. Defaults to TRUE.
#' @param nb.plot The number of plots to display.
#' @param beside Logical, if TRUE, the bar plot is displayed horizontally.
#'   Defaults to TRUE.
#' @param las The style of axis labels on the bar plots. Default is 1.
#' @param ... Additional arguments to control the appearance of the bar plots.
#'
#' @importFrom grDevices heat.colors
#'
#' @return A data frame with the summed square proportions.
#'
#' @export
showAOV <- function(aAOV_DEXi,
                    main.show = T,
                    nb.plot = 8,
                    beside = T,
                    las = 1,
                    ...) {
    # x11()
    par(mfrow = c(2, 2))
    #Total complet
    out <- aAOV_DEXi[[1]]
    outgraph <- out[rev(1:nrow(out)), ]
    outnames <- rownames(outgraph)
    outnames <- gsub("([ ])", "", outnames)
    if (main.show == F) {
        barplot(outgraph$ss.ratio,
                horiz = T,
                names.arg = outnames,
                main = "Total Sums of Squares",
                las = las,
                ...)
    } else {
        outgraph.2 <- rbind(outgraph$main.ss.ratio,
                            outgraph$ss.ratio - outgraph$main.ss.ratio)
        barplot(outgraph.2,
                horiz = T,
                names.arg = outnames,
                main = "Main-effect and total Sums of Squares",
                col = heat.colors(2),
                beside = F,
                las = las,
                ...)
    }
    #Effect complet
    out <- aAOV_DEXi[[2]]
    # sélection des principaux effets
    outgraph <- out[rev(1:min(nb.plot,nrow(out))), ]
    outnames <- rownames(outgraph)
    outnames <- gsub("([ ])", "", outnames)
    # calcul des SS cumulées
    temp <- outgraph$ss.ratio
    temp2 <- rev(cumsum(rev(temp)))
    # graphiques
    if (beside == F) {
        outgraph.2 <- rbind(outgraph$ss.ratio,
                            temp2 - temp)
    } else {
        outgraph.2 <- rbind(outgraph$ss.ratio, temp2)
    }
    barplot(outgraph.2,
            horiz = T,
            names.arg = outnames,
            main = "Sums of Squares proportions",
            col = heat.colors(2),
            beside = beside,
            las = las,
            ...)
    #Total power 2
    out <- aAOV_DEXi[[3]]
    outgraph <- out[rev(1:nrow(out)), ]
    outnames <- rownames(outgraph)
    outnames <- gsub("([ ])", "", outnames)
    if (main.show==F) {
        barplot(outgraph$ss.ratio,
                horiz = T,
                names.arg = outnames,
                main = "Total Sums of Squares",
                las = las,
                ...)
    } else {
        outgraph.2 <- rbind(outgraph$main.ss.ratio,
                            outgraph$ss.ratio - outgraph$main.ss.ratio)
        barplot(outgraph.2,
                horiz = T,
                names.arg = outnames,
                main = "Main-effect and total Sums of Squares",
                col = heat.colors(2),
                beside = F,
                las = las,
                ...)
    }
    #Effect complet
    out <- aAOV_DEXi[[4]]
    # sélection des principaux effets
    outgraph <- out[rev(1:min(nb.plot, nrow(out))), ]
    outnames <- rownames(outgraph)
    outnames <- gsub("([ ])", "", outnames)
    # calcul des SS cumulées
    temp <- outgraph$ss.ratio
    temp2 <- rev(cumsum(rev(temp)))
    # graphiques
    if (beside == F) {
        outgraph.2 <- rbind(outgraph$ss.ratio,
                            temp2 - temp)
    } else {
        outgraph.2 <- rbind(outgraph$ss.ratio, temp2)
    }
    barplot(outgraph.2,
            horiz = T,
            names.arg = outnames,
            main = "Sums of Squares proportions",
            col = heat.colors(2),
            beside = beside,
            las = las,
            ...)

    out
}


#' Calculate AOV results
#'
#' Calculates the results of an Analysis of Variance (AOV).
#'
#' @param aTree A decision tree object to perform the analysis on.
#'
#' @importFrom stats aov as.formula
#'
#' @return A list with the results of the AOV.
#'
#' @export
AOV_DEXi <- function(aTree) {
    # Create the simulation plan
    leavesOptionIndices <- aTree@Nodes %>%
        sapply(function(x) { if (x@IsLeaf) {x@RangeScale} }) %>%
        unlist()

    factorialPlan <- t(gen.factorial(as.numeric(leavesOptionIndices),
                                     center = FALSE))
    rownames(factorialPlan) <- aTree@Leaves
    #Perform the simulation
    myResults <- sapply(1:dim(factorialPlan)[2],
                        function(x) {
                            EvaluateScenario(aTree,
                                             as.matrix(factorialPlan[, x]))
                        })
    myResults <- myResults[c(aTree@RootName, aTree@Leaves), ]
    #Transform the matrix for AOV
    myResults <- as.data.frame(t(myResults))
    for(i in 1:aTree@NumberOfLeaves) {
        myResults[[i+1]] <- factor(myResults[[i+1]])
    }

    # Need to abbreviate the names ... a 4 letters abbreviations
    colnames(myResults) <- abbreviate(colnames(myResults),
                                      minlength = 4,
                                      dot = FALSE)
    #Create the formula
    xNames <- paste(colnames(myResults)[2:(aTree@NumberOfLeaves+1)],
                    collapse = "+")
    myFormula <- as.formula(paste(colnames(myResults)[1],
                                  " ~ ",
                                  paste(xNames, collapse = "+")))
    # Perform the AOV: effet de premier ordre
    myResults.aov <- aov(myFormula, data = myResults)
    out <- list()
    out[[1]] <- round(sensib.total(myResults.aov), 3)
    out[[2]] <- round(sensib.effet(myResults.aov), 3)
    # Take into account interaction order 2
    myFormula2 <- as.formula(paste(colnames(myResults)[1],
                                   " ~ ", "(",
                                   paste(xNames, collapse= "+"), ")^2"))
    myResults2.aov <- aov(myFormula2, data = myResults)
    out[[3]] <- round(sensib.total(myResults2.aov), 3)
    out[[4]] <- round(sensib.effet(myResults2.aov), 3)
    return(out)
}
