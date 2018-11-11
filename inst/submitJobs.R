library(batchtools)
args = commandArgs(trailingOnly=TRUE)

regFileDir <- args[1]
mainFileDir <- args[2]
configFileDir <- paste0(mainFileDir,"/slurm.conf.R")

reg <- loadRegistry(regFileDir, conf.file=configFileDir, writeable=T)

additionalParameters <- readRDS(file = paste0(reg$file.dir, "/additionalParameters.rds"))

do.call(batchtools::submitJobs, args = additionalParameters)
