path <- "#1#"

cluster.functions = batchtools::makeClusterFunctionsSlurm(paste0(path,"/slurm.tmpl"),
                                                          nodename = "localhost")
default.resources = list(walltime = 4*24*60*60, memory = 1024, ncpus = 1)
source = union(source, "sources.R")
