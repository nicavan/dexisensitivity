#' Title
#'
#' @param aTree aTree
#' @param iTest iTest
#'
#'
#' @return
#' @export
#'
#' @examples
infoAOV <- function(aTree, iTest = 50) {
    # Compute the size of the factorial plan and the time requested to run it
    preprod <- aTree@Nodes %>%
        sapply(function(x) {if (x@isLeaf) {x@rangeScale}}) %>%
        unlist() %>%
        cumprod()
    nbRuns <- preprod[aTree@nbLeaves]

    start_time <- Sys.time()
    option <- matrix(nrow = aTree@nbLeaves,
                     ncol = iTest)
    rownames(option) <- aTree@Leaves
    for(k in aTree@Leaves) {
        option[k,] <- sample(aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@rangeScale,
                             size = iTest,
                             prob = aTree@Nodes[[getID(aTree@Nodes, k)[1]]]@Proba,
                             replace = TRUE)
    }

    dummy <- sapply(1:iTest,
                    function(x) {
                        EvaluateScenario(aTree, as.matrix(option[, x]))
                    })
    end_time <- Sys.time()
    cat("\n", aTree@nbLeaves, " factors",
        "\n Approximative required time to run the ", nbRuns, " modalities",
        (end_time-start_time)*nbRuns/iTest/60, " minutes")
}


#' Title
#'
#' Fonction de calcul des indices de sensibilite factoriels
#'
#' @param aov.obj objet "aov" issu de l'analyse de variance des donnees de simulation
#' @param nb.plot nombre d'effets repr?sent?s sur le graphique
#' @param beside voir la doc de barplot
#' @param las las
#' @param ... ...
#'
#' @return
#' @export
#'
#' @examples
sensib.effet <- function(aov.obj,
                         nb.plot = 8,
                         beside = T,
                         las = 1,
                         ...) {

    #### PRELIMINAIRES: récupération des résultats d'anova
    # indic.fact: matrice 0-1 de correspondance facteurs*termes-du-modèle
    indic.fact <- attr(aov.obj$terms, "factors")[-1, ]
    # aov.df: vecteur des Degrés de Liberté, résiduelle comprise
    # aov.ss: vecteur des Sommes de Carrés, résiduelle comprise
    # aov.cm: vecteur des Carrés Moyens, résiduelle comprise
    ### ATTENTION ###
    # sous S:
    #aov.summ <- summary(aov.obj)
    #aov.ss <- aov.summ[,"Sum of Sq"]
    # sous R:
    aov.summ <- summary(aov.obj)[[1]]
    aov.ss <- aov.summ[, "Sum Sq"]
    ### FIN DE "ATTENTION" ###
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

    #---------------------------------------------------------------------------
    #### Critères de sensibilité terme par terme
    #---------------------------------------------------------------------------
    # sorties
    out <- data.frame(df = aov.df, ss = aov.ss, ss.ratio = aov.ss/tss,
                      cm = aov.cm, F = aov.cm/rms)
    rownames(out) <- rownames(aov.summ)
    out <- out[rev(order(out$ss.ratio)), ]
}


#' Title
#'
#' Fonction de calcul des indices de sensibilite factoriels
#'
#' @param aov.obj objet "aov" issu de l'analyse de variance des donnees de simulation
#' @param fact.interet fact.interet
#' @param las las
#' @param ... ...
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' Fonction de calcul des indices de sensibilite factoriels
#'
#' @param aov.obj objet "aov" issu de l'analyse de variance des donnees de simulation
#' @param main.show main.show
#' @param las las
#' @param ... ...
#'
#' @return
#' @export
#'
#' @examples
sensib.total <- function(aov.obj,
                         main.show = T,
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


#' Title
#'
#' @param aAOV_DEXi aAOV_DEXi
#' @param main.show main.show
#' @param nb.plot nb.plot
#' @param beside beside
#' @param las las
#' @param ... ...
#'
#' @importFrom grDevices heat.colors
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @param aTree aTree
#'
#' @importFrom stats aov as.formula
#'
#' @return
#' @export
#'
#' @examples
AOV_DEXi <- function(aTree) {
    # Create the simulation plan
    leavesOptionIndices <- aTree@Nodes %>%
        sapply(function(x) { if (x@isLeaf) {x@rangeScale} }) %>%
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
    myResults <- myResults[c(aTree@rootName, aTree@Leaves), ]
    #Transform the matrix for AOV
    myResults <- as.data.frame(t(myResults))
    for(i in 1:aTree@nbLeaves) {
        myResults[[i+1]] <- factor(myResults[[i+1]])
    }

    # Need to abbreviate the names ... a 4 letters abbreviations
    colnames(myResults) <- abbreviate(colnames(myResults),
                                      minlength = 4,
                                      dot = FALSE)
    #Create the formula
    xNames <- paste(colnames(myResults)[2:(aTree@nbLeaves+1)], collapse = "+")
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
