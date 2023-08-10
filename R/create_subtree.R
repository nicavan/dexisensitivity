#' Constructs a subtree from a given node in a tree
#'
#' Constructs a subtree starting from a specific node in a provided tree
#'
#' @param aTree A "Tree" object
#' @param nodeName The name of the node from which to start constructing the
#'   subtree
#' @param avoidrep A logical value indicating whether to avoid repeated nodes
#'   (Default: FALSE)
#'
#' @return A "Tree" object representing the subtree
#'
#' @export
createSubTree <- function(aTree, nodeName, avoidrep = F) {

    # On récupère l'ID du noeud
    id <- get_id(aTree@Nodes, nodeName)

    # Si on est dans le cas d'une Leaf-Aggregated !
    # on récupère le noeud aggrégé et non le noeud feuille
    if (aTree@IsLeafAggregated) {
        id <- id %>%
            sapply(function(x) {
                if (!aTree@Nodes[[x]]@IsLeaf) {aTree@Nodes[[x]]@Id}
            }) %>%
            unlist()
    }

    # to avoid repeted branch
    if (length(id)>1 & avoidrep) {
        id <- id[1]
    }

    # On récupère les noeuds qui possèdent le nodePath du noeud ou l'on coupe
    l.Attributes <- aTree@Nodes %>%
        sapply(function(x) {
            if (grep(paste(aTree@Nodes[[id]]@NodePath, collapse = " "),
                     paste(x@NodePath, collapse = " "),
                     fixed=TRUE) %>%
                length()) {x@Id}
        }) %>%
        unlist()

    NumberOfAttributes <- length(l.Attributes)

    # On récupère les informations pour créer l'arbre et modifier les noeuds.
    l.Leaves <- l.Attributes %>%
        sapply(function(x) {
            if (aTree@Nodes[[x]]@IsLeaf) {aTree@Nodes[[x]]@Id}
        }) %>%
        unlist()

    l.Aggregated <- l.Attributes %>%
        sapply(function(x) {
            if (!aTree@Nodes[[x]]@IsLeaf) {aTree@Nodes[[x]]@Id}
        }) %>%
        unlist()

    # On crée le nouvel arbre
    Paths <- l.Attributes %>%
        lapply(function(x) {
            aTree@Nodes[[x]]@NodePath[aTree@Nodes[[id]]@Depth:length(aTree@Nodes[[x]]@NodePath)]
        })

    TreeNodes <- aTree@Nodes[l.Attributes]
    for(i in 1:NumberOfAttributes) {
        TreeNodes[[i]]@Id <- i
        TreeNodes[[i]]@NodePath <- Paths[[i]]
        TreeNodes[[i]]@Depth <- length(TreeNodes[[i]]@NodePath)
        TreeNodes[[i]]@IsLeafAndAggregated <- FALSE
        TreeNodes[[i]]@Twin <- integer(0)
        if (i == 1) {TreeNodes[[i]]@Mother <- ""}
    }

    l.Leaves <- TreeNodes %>%
        sapply(function(x) {
            if (x@IsLeaf) {x@Name}
        }) %>%
        unlist()

    l.Aggregated <- TreeNodes %>%
        sapply(function(x) {
            if (!x@IsLeaf) {x@Name}
        }) %>%
        unlist()

    # Deal with Twins and LeafAggregated
    if (aTree@IsMultiple) {
        Multiple <- table(l.Leaves)
        if (max(Multiple) > 1) {
            Multiple <- as.matrix(Multiple[Multiple > 1])
            colnames(Multiple) <- "Occ"

            for(i in 1:dim(Multiple)[1]) {
                dup <- get_id(TreeNodes, rownames(Multiple)[i])
                for(j in 1:length(dup)) {
                    TreeNodes[[dup[j]]]@Twin <- dup[-c(which(is.element(dup,TreeNodes[[dup[j]]]@Id)))]
                }
            }

            isMultiple <- T
        } else {
            Multiple <- data.frame(Occ = NA)
            isMultiple <- F
        }
    } else {
        Multiple <- data.frame(Leaf = NA, Occ = NA)
        isMultiple <- F
    }

    if (aTree@IsLeafAggregated) {
        l.LeafAggregated <- intersect(l.Leaves, l.Aggregated)
        if (length(l.LeafAggregated)) {
            isLeafAggregated <- T

            for(i in 1:length(l.LeafAggregated)) {
                dup <- get_id(TreeNodes, l.LeafAggregated[i])
                for(j in 1:length(dup)) {
                    TreeNodes[[dup[j]]]@Twin <- dup[-c(which(is.element(dup,TreeNodes[[dup[j]]]@Id)))]
                    TreeNodes[[dup[j]]]@IsLeafAndAggregated <- T
                }
            }
        } else {
            isLeafAggregated <- F
        }
    } else {
        isLeafAggregated <- F
        l.LeafAggregated <- ""
    }

    l.Leaves <- unique(l.Leaves)

    if (isLeafAggregated) {
        l.Leaves <- l.Leaves[-c(which(is.element(l.Leaves, l.LeafAggregated)))]
    }

    nbLeaves <- length(l.Leaves)
    nbLevels <- Paths %>%
        sapply(function(x) {length(x)}) %>%
        max()
    l.Attributes <- l.Attributes %>%
        sapply(function(x) {aTree@Nodes[[x]]@Name})

    out <- new("Tree",
               RootName = nodeName,
               NumberOfAttributes = length(TreeNodes),
               NumberOfLeaves = nbLeaves,
               Depth = nbLevels,
               Nodes = TreeNodes,
               Multiple = as.data.frame(Multiple),
               IsMultiple = isMultiple,
               IsLeafAggregated = isLeafAggregated,
               LeafAggregated = l.LeafAggregated,
               Attributes = l.Attributes,
               Leaves = l.Leaves,
               Aggregated = l.Aggregated,
               EvaluationOrder = numeric(0),
               Paths = Paths)

    out@EvaluationOrder <- EvaluateOrder(out)

    return(out)
}
