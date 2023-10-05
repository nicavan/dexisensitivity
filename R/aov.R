#' Estimates the execution time for factorial simulations
#'
#' Estimates the execution time for a specified number of factorial simulations
#' based on the time taken to run a subset of simulations.
#'
#' @param tree The Tree object on which simulations are run.
#' @param test_runs Number of simulations to be used for time estimation.
#'   Default is 50.
#'
#' @return No explicit return. Prints the estimated execution time.
#'
#' @export
estimate_aov_time <- function(tree, test_runs = 50) {

  # Compute the total number of simulations based on tree leaves
  total_simulations <- tree@Nodes %>%
    sapply(function(node) {
      if (node@IsLeaf) node@RangeScale
    }) %>%
    unlist() %>%
    prod()

  # Generate sample scenarios
  sample_scenarios <- matrix(nrow = tree@NumberOfLeaves,
                             ncol = test_runs)
  rownames(sample_scenarios) <- tree@Leaves

  sample_scenarios[] <- sapply(tree@Leaves, function(leaf) {
    node <- tree@Nodes[[get_id(tree@Nodes, leaf)[1]]]
    sample(node@RangeScale, size = test_runs,
           prob = node@Probability, replace = TRUE)
  })

  # Measure time taken for the subset of simulations
  start_time <- Sys.time()
  apply(sample_scenarios, 2, function(scenario) {
    evaluate_scenario(tree, as.matrix(scenario))
  })
  end_time <- Sys.time()
  elapsed_time <- difftime(end_time, start_time, units = "mins")

  # Estimate total execution time
  estimated_time_minutes <- elapsed_time * total_simulations / test_runs
  cat("\n", tree@NumberOfLeaves, " factors",
      "\n Approximate time to run the ", total_simulations, " modalities: ",
      estimated_time_minutes, " minutes")
}


#' Calculate AOV Results
#'
#' Provides the results of an Analysis of Variance (AOV) based on a given tree.
#'
#' @param tree Tree object for analysis.
#'
#' @importFrom stats aov as.formula
#'
#' @return A list containing AOV results.
#'
#' @export
aov_tree <- function(tree) {
  # Create the factorial plan
  factorial_plan <- create_factorial_plan(tree)

  # Simulate scenarios based on the plan
  results <- sapply(1:dim(factorial_plan)[2], function(x) {
    evaluate_scenario(tree, as.matrix(factorial_plan[, x]))
  })
  results <- results[c(tree@RootName, tree@Leaves), ]

  # Convert results matrix to data frame and factorize
  results_df <- as.data.frame(t(results))
  for (i in 1:tree@NumberOfLeaves) {
    results_df[[i+1]] <- factor(results_df[[i+1]])
  }

  # Abbreviate column names
  colnames(results_df) <- abbreviate(colnames(results_df),
                                     minlength = 4, dot = FALSE)

  # Generate AOV formulas
  formula_1 <- generate_aov_formula(results_df, order = 1)
  formula_2 <- generate_aov_formula(results_df, order = 2)

  # AOV for first-order effects
  aov_results_1 <- aov(formula_1, data = results_df)
  output <- list()
  output[[1]] <- round(sensib.total(aov_results_1), 3)
  output[[2]] <- round(compute_aov_sensitivity_effects(aov_results_1), 3)

  # AOV considering 2nd order interactions
  aov_results_2 <- aov(formula_2, data = results_df)
  output[[3]] <- round(sensib.total(aov_results_2), 3)
  output[[4]] <- round(compute_aov_sensitivity_effects(aov_results_2), 3)

  return(output)
}


#' Create Factorial Plan from Tree
#'
#' Extracts leaf indices from the tree and creates the factorial plan based on
#' gen.factorial function.
#'
#' @param tree Tree object for analysis.
#'
#' @importFrom AlgDesign gen.factorial
#'
#' @return A matrix representing the factorial plan.
create_factorial_plan <- function(tree) {
  # Extract leaf indices from the tree
  leaf_indices <- tree@Nodes %>%
    sapply(function(node) {
      if (node@IsLeaf) {
        node@RangeScale
      }
    }) %>%
    unlist()

  # Generate factorial plan
  factorial_plan <- t(gen.factorial(as.numeric(leaf_indices), center = FALSE))
  rownames(factorial_plan) <- tree@Leaves

  return(factorial_plan)
}


#' Generate AOV Formula
#'
#' Constructs an Analysis of Variance (AOV) formula based on the results
#' dataframe.
#'
#' @param results_df Data frame containing AOV results.
#' @param order Integer representing the order of interaction; 1 for
#'   first-order, 2 for second-order.
#'
#' @return A formula object for AOV.
generate_aov_formula <- function(results_df, order = 1) {
  # Construct AOV formula based on the order
  predictors <- paste(colnames(results_df)[2:ncol(results_df)], collapse = "+")
  if (order == 2) {
    formula_str <- paste(colnames(results_df)[1], "~", "(", predictors, ")^2")
  } else {
    formula_str <- paste(colnames(results_df)[1], "~", predictors)
  }

  return(as.formula(formula_str))
}


#' Calculate sensitivity criteria for model terms
#'
#' Provides sensitivity criteria for each term in a fitted model.
#'
#' @param aov_obj An object of class `aov` from a call to `aov()`.
#'
#' @return A data frame with sensitivity criteria for each term in the model.
#'   The columns of the data frame include:
#'   - `df`: degrees of freedom
#'   - `ss`: sum of squares
#'   - `ss.ratio`: ratio of sum of squares to total sum of squares
#'   - `cm`: mean square (or variance) for each term
#'   - `F`: F-statistic for each term
#'
#'   Rows are ordered in decreasing order of ss.ratio.
compute_aov_sensitivity_effects <- function(aov_obj) {
  # Ensure the object is of class 'aov'
  if(!inherits(aov_obj, "aov")) {
    stop("Provided object isn't of class 'aov'")
  }

  # ANOVA results extraction
  aov_summary <- summary(aov_obj)[[1]]
  sum_squares <- aov_summary[, "Sum Sq"]
  degrees_freedom <- aov_summary[, "Df"]
  mean_squares <- sum_squares/degrees_freedom

  # Total sum of squares, degrees of freedom, and mean squares
  total_ss <- sum(sum_squares)
  total_df <- sum(degrees_freedom)
  total_ms <- total_ss/total_df

  # Residual sum of squares, degrees of freedom, and mean squares
  residual_df <- aov_obj$df.residual
  if (residual_df > 0) {
    residual_ss <- sum_squares[length(sum_squares)]
    residual_ms <- residual_ss/residual_df
  } else {
    residual_ss <- NA
    residual_ms <- NA
  }

  # Calculate sensitivity criteria for each term
  output <- data.frame(df = degrees_freedom, ss = sum_squares,
                       ss.ratio = sum_squares/total_ss, cm = mean_squares,
                       F = mean_squares/residual_ms)

  rownames(output) <- rownames(aov_summary)
  output <- output[rev(order(output$ss.ratio)), ]

  return(output)
}


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



