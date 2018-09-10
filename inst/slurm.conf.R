args = commandArgs(trailingOnly=TRUE)
if(length(args) > 0){
    path <- args[1]
}else{
    path <- "#1#"
}

cluster.functions = batchtools::makeClusterFunctionsSlurm(paste0(path,"/slurm.tmpl"),
                                                          nodename = "localhost")
default.resources = list(walltime = 1*24*60*60, memory = 1024, ncpus = 1)
source = union(source, "sources.R")
