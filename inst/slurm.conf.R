load(file = "~/batchExperiments/config.rda")

cluster.functions = batchtools::makeClusterFunctionsSlurm("~/batchExperiments/slurm.tmpl", nodename = config.list$nodeName)
default.resources = list(walltime = 1*24*60*60, memory = 1024, ncpus = 1)
