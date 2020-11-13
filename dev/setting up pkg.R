

# generate collate file for DESCRIPTION ----------------------------------------
rm(fls)
fls = paste(
  dir(path = "R", pattern="\\.[rR]$", recursive=TRUE)
  , collapse=" ")

cat(strwrap(sprintf("Collate: %s", fls), exdent=4), sep="\n")
