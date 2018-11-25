if (!require("CEGO",character.only = TRUE))
{
    stop("Something went wrong in the setup, Please install CEGO on the cluster!!")
}
if (!require("SPOT",character.only = TRUE))
{
    stop("Something went wrong in the setup, Please install SPOT on the cluster!!")
}
if (!require("smoof",character.only = TRUE))
{
    stop("Something went wrong in the setup, Please install smoof on the cluster!!")
}
if (!require("cmaes",character.only = TRUE))
{
    stop("Something went wrong in the setup, Please install cmaes on the cluster!!")
}

sourceDir <- "#1#"
path <- paste0(sourceDir,"/R/")
files <- dir(path)
for(file in files){
    source(paste0(path,file))
}
for(file in files){
    source(paste0(path,file))
}
