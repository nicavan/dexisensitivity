## code to prepare `masc2` dataset

# get model from .dxi file
# /!\ Adjust path /!\
model_path <- "[PATH to adjust]/dexiranalysis/inst/extdata/arborescence_MASC_2_0.dxi"
MT <- XML::xmlTreeParse(model_path,
                        useInternalNodes = T)

# - Create the Tree
list_masc2 <- create_tree(MT)
masc2 <- list_masc2[[1]]

usethis::use_data(masc2, overwrite = TRUE)
