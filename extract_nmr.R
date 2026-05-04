nmr  <- function(x) {
#  take a GAMESS (US) log file after an NMR run and extracts the chemical shifts by
#  inserting the name of the file between quotes in the function
#  extract portion of file between ix[1] and ix[2]
L <- readLines(x)
ix <- grep("GIAO CHEMICAL SHIELDING TENSOR|DONE WITH NMR SHIELDING", L)
L0 <- L[seq(ix[1]+1, ix[2]-1)]

# extract fields
g <- grep("^[  (]*\\d", L0, value = TRUE)
#res <- gsub(" ", "", paste(substr(g, 1, 3), substr(g, 58, 68)))
res <- gsub(" ", "", paste(substr(g, 4, 7), substr(g, 66, 74)))
m <- matrix(res, ncol = 3, byrow = TRUE)
m  <- data.frame(atom = m[, 1], isotropic.S = as.numeric(m[,2]), anisitropy = as.numeric(m[,3]), stringsAsFactors=FALSE)
return(m)
}
