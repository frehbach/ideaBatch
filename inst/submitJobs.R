library(batchtools)
args = commandArgs(trailingOnly=TRUE)

regFileDir <- args[1]
mainFileDir <- args[2]
configFileDir <- paste0(mainFileDir,"/slurm.conf.R")

loadRegistry(regFileDir, conf.file=configFileDir, writeable=T)
submitJobs()
