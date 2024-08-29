## code to prepare `dexifruits_v1` dataset

# get model from .dxi file
# /!\ Adjust path /!\
model_path <- "[PATH to Adjust]/dexiranalysis/inst/extdata/DEXiFruits_V1.dxi"
MT <- XML::xmlTreeParse(model_path,
                        useInternalNodes = T)

# - Create the Tree
list_dexifruits_v1 <- create_tree(MT, which_root = 1, correct = TRUE)
dexifruits_v1 <- list_dexifruits_v1[[1]]

usethis::use_data(dexifruits_v1, overwrite = TRUE)
