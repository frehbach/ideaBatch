if (!require("CEGO",character.only = TRUE))
{
    install.packages("CEGO",dep=TRUE)
    require(CEGO)
}
if (!require("SPOT",character.only = TRUE))
{
    install.packages("SPOT",dep=TRUE)
    require(SPOT)
}
if (!require("smoof",character.only = TRUE))
{
    install.packages("smoof",dep=TRUE)
    require(smoof)
}
if (!require("cmaes",character.only = TRUE))
{
    install.packages("cmaes",dep=TRUE)
    require(cmaes)
}

sourceDir <- getSrcDirectory(function(dummy) {dummy})
path <- paste0(sourceDir,"/Sources/")
files <- dir(path)
for(file in files){
    source(paste0(path,file))
}
for(file in files){
    source(paste0(path,file))
}
