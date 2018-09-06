load(paste0(system.file(package = "ideaBatch"),"/config.rda"))

cluster.functions = batchtools::makeClusterFunctionsSlurm(paste0(idea.config.list$desiredDir,"/slurm.tmpl"), nodename = idea.config.list$nodeName)
default.resources = list(walltime = 1*24*60*60, memory = 1024, ncpus = 1)
