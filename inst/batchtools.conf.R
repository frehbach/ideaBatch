path <- "#1#"
setwd(path)
cluster.functions = batchtools::makeClusterFunctionsInteractive()
source = union(source, "sources.R")
