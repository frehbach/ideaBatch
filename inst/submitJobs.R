library(batchtools)
args = commandArgs(trailingOnly=TRUE)

regFileDir <- args[1]
mainFileDir <- args[2]
configFileDir <- paste0(configFileDir,"slurm.conf.R")
sourceFileDir <- paste0(configFileDir,"sources.R")

source(sourceFileDir)
loadRegistry(regFileDir, conf.file=configFileDir, writeable=T)
submitJobs()
