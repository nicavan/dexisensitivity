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
#' @importFrom magrittr %>%
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

        # aTree@EvalOrder <- EvaluateOrder(aTree)

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
createNode <- function(listeNoeuds, MT)
{
    #Is it a leaf?
    # On check chaque embranchements potentiels. Si c'est un noeux alors FALSE, si c'est une fin alors TRUE (leaf)
    isLeaf <- ifelse(length(sapply(XML::getNodeSet(MT,paste(getChaine(listeNoeuds),"/FUNCTION",sep="")),XML::xmlSize)),F,T)
    #Children
    # Si Leaf alors ça n'a pas de filles. Sinon, on cherche le nom des attributs en dessous (ceux des filles)
    l.Children <- if(isLeaf) vector(mode="character",length=0) else sapply(XML::getNodeSet(MT,paste(getChaine(listeNoeuds),"/ATTRIBUTE/NAME",sep="")),XML::xmlValue)
    #Mother if any
    # Si listeNoeuds (paths) = 1 alors c'est le noeud originel
    # Sinon, on prend le nom de l'vant dernier attribut (celui du noeud père)
    mother<-ifelse(length(listeNoeuds)>1,listeNoeuds[length(listeNoeuds)-1],character(0))
    #Sisters: they do have the same mother
    # Si noeud originel (length = 1) alors ya pas de soeurs
    # Sinon, on récère les attributs des filles de la mère et on retire celui de l'individu (on veut que ses soeurs)
    l.Sisters <- if(length(listeNoeuds)>1) sapply(XML::getNodeSet(MT,paste(getChaine(listeNoeuds[1:length(listeNoeuds)-1]),"/ATTRIBUTE/NAME",sep="")),XML::xmlValue) else vector(mode="character",length=0)
    l.Sisters<-l.Sisters[l.Sisters[]!=listeNoeuds[length(listeNoeuds)]]
    #Scale and labels
    # On récupère des valeurs dans l'arbre
    scaleNode<-length(sapply(XML::getNodeSet(MT,paste(getChaine(listeNoeuds),"/SCALE/SCALEVALUE",sep="")),XML::xmlSize))
    scaleLabel<-sapply(XML::getNodeSet(MT,paste(getChaine(listeNoeuds),"/SCALE/SCALEVALUE/NAME",sep="")),XML::xmlValue)
    #Aggregation function
    # Création de la table d'aggrégation
    if(!isLeaf)
    {
        c.Function<-sapply(XML::getNodeSet(MT,paste(getChaine(listeNoeuds),"/FUNCTION/LOW",sep="")),XML::xmlValue)
        #Transform as a vector
        nbChar<-nchar(c.Function)
        v.Function<-numeric(nbChar)
        for(i in 1:nbChar)
        {
            #Modify attribute 0...n to 1...n+1
            v.Function[i]<-as.numeric(substr(c.Function,i,i))+1
        }
        #Scales from nodes n-1
        nbChildren <- length(l.Children)
        scaleChildren <- numeric(nbChildren)
        for(i in 1:nbChildren)
        {
            scaleChildren[i]<-length(sapply(XML::getNodeSet(MT,paste(getChaine(c(listeNoeuds,l.Children[i])),"/SCALE/SCALEVALUE",sep="")),XML::xmlSize))
        }
        #Create the factorial plan
        if(nbChildren==1)
        {
            aggregation <- (1:scaleChildren)
        }
        else
        {
            factorialPlan<-rev(AlgDesign::gen.factorial(rev(as.numeric(scaleChildren)),center=FALSE))
            nbFactorialPlan<-dim(factorialPlan)[1]
            aggregation <- as.matrix(factorialPlan[,seq(ncol(factorialPlan))])
        }
        aggregation <- cbind(aggregation,v.Function)
        colnames(aggregation) <- c(l.Children, listeNoeuds[length(listeNoeuds)])
    }
    else
        aggregation<-as.matrix(0)
    #Create the weights (equal weights if not defined)##############################
    # Gestion du poids des attributs
    WeightList<-numeric(scaleNode)
    if(isLeaf)
        WeightList <- rep(1/scaleNode,scaleNode)
    else
    {
        if(nbChildren==1)
            WeightList <- 100
        else
        {
            if(length(sapply(XML::getNodeSet(MT,paste(getChaine(listeNoeuds),"/FUNCTION/WEIGHTS",sep="")),XML::xmlValue)))
                WeightList <- as.numeric(unlist(strsplit(sapply(XML::getNodeSet(MT,paste(getChaine(listeNoeuds),"/FUNCTION/WEIGHTS",sep="")),XML::xmlValue),";")))
            else
                WeightList <- -1
        }
    }
    # Création sortie (le noeud)
    out <- new("Node",name= listeNoeuds[length(listeNoeuds)], Depth=length(listeNoeuds),isLeaf=isLeaf,
               mother=mother, sisters=l.Sisters, children=l.Children,
               aggregation=aggregation, rangeScale=scaleNode, scaleLabel=scaleLabel,
               Proba=WeightList,nodePath=listeNoeuds)
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
getID <- function(listNodes,nodeName)
{
    out <- numeric(0)
    for(i in 1:length(listNodes))
    {
        if(rev(listNodes[[i]]@nodePath)[1]==nodeName)
            out <- c(out,listNodes[[i]]@id)
    }
    return(out)
}
