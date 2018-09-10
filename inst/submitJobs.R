library(batchtools)
args = commandArgs(trailingOnly=TRUE)

regFileDir <- args[1]
configFileDir <- args[2]

loadRegistry(regFileDir, conf.file=configFileDir, writeable=T)
submitJobs()
