
#' Title
#'
#' @return
#' @export
#'
#' @import batchtools
#' @import ssh
initBatchTools <- function() {
    userName <- readline("Please specify your username on the cluster:")

    nodeName <- readline("Servername in ssh config?")
    print("In which directory would you like to use batchtools?")
    print("The folder will be synchronized to the cluster in the same path relative to your user folder")
    print("Thus, if using batchtools in ~/Desktop/batchtools a folder will be created on the cluster in your home dir:")
    print("e.g. : /home/0/#YOUR_ID#/Desktop/batchtools")
    print("The directory has to start with ~/")
    desiredDir <- readline("desired directory:")

    if(!startsWith(desiredDir,"~/")){
        stop("Wrong path specified")
    }

    if(endsWith(desiredDir,"/")){
        desiredDir <- substring(desiredDir,1,nchar(desiredDir)-1)
    }

    dir.create(desiredDir, showWarnings = FALSE)
    file.copy(system.file("", "slurm.tmpl", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "slurm.conf.R", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "packageInstaller.R", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "sources.R", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "submitJobs.R", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "Sources/", package = "ideaBatch"), desiredDir, recursive = T)
    dir.create(paste0(desiredDir ,"/Experiments"), showWarnings = FALSE)

    sshInfo <- readSSHInfo(nodeName)
    sshKey <- sshInfo$identFile

    sess <- ssh::ssh_connect(host = paste0(sshInfo$user,"@",sshInfo$hostName),keyfile = sshInfo$identFile)
    userDir <- rawToChar(ssh::ssh_exec_internal(session = sess, "pwd")$stdout)
    userDir <- strsplit(userDir,"/")[[1]][4]
    userDir <- substring(userDir,first = 1, last = nchar(userDir) - 1)

    idea.config.list <- list("userName" = userName, "sshKey" = sshKey, "nodeName" = nodeName,
                             "desiredDir" = desiredDir, "userDir" = userDir)
    save(idea.config.list,file = paste0(system.file(package = "ideaBatch"),"/config.rda"))
    file.copy(system.file("", "config.rda", package = "ideaBatch"), desiredDir)


    dirExtern <- substring(desiredDir, 3, nchar(desiredDir))
    insertPathToTemplate(paste0(desiredDir,"/slurm.tmpl"), c(userDir,dirExtern))
    insertPathToTemplate(paste0(desiredDir,"/slurm.conf.R"), c(desiredDir))
    insertPathToTemplate(paste0(desiredDir,"/sources.R"), c(desiredDir))

    ssh::ssh_exec_wait(session = sess, paste0("mkdir -p /home/0/",userDir ,"/", dirExtern))
    synchronizeFolder()

    ssh::ssh_exec_wait(session = sess, paste0("/opt/software/R/R-current/bin/Rscript ", desiredDir,"/packageInstaller.R"))
}

insertPathToTemplate <- function(fullPath, paramVector)
{
    lines = readLines(fullPath)
    paramIndex = 1
    for(p in paramVector)
    {
        lines = gsub(paste("#", as.character(paramIndex),"#",sep=""),p,lines)
        paramIndex = paramIndex + 1
    }
    cat(lines,file = fullPath,sep = "\n")
}

readSSHInfo <- function(nodeName){
    sshConfig <- read.table("~/.ssh/config",sep=" ",blank.lines.skip = F)
    hostIndex <- which(sshConfig$V2 == nodeName)
    sshConfig$tbl_id <- cumsum((sshConfig$V1 == ""))
    sshConfig <- sshConfig[which(sshConfig$tbl_id == sshConfig$tbl_id[hostIndex]),]
    sshConfig$V1 <- tolower(sshConfig$V1)

    hostName <- as.character(sshConfig$V2[which(sshConfig$V1 == "hostname")])
    identFile <- as.character(sshConfig$V2[which(sshConfig$V1 == "identityfile")])
    user <- as.character(sshConfig$V2[which(sshConfig$V1 == "user")])

    return(list("hostName" = hostName, "identFile" = identFile, "user" = user))
}

#' Synchronize Experiment Folder
#'
#' Synchronizes the Experiment folder to the cluster
#'
#' @param dir the dir that is synchronized
synchronizeFolder <- function(doDelete = T) {
    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))
    dir <- idea.config.list$desiredDir
    dirLocal <- dir
    dirExtern <- substring(dir, 3, nchar(dir))

    if(doDelete){
        system(paste0("rsync -r -a --delete -e ssh ", dirLocal,"/ owos:/home/0/",idea.config.list$userDir ,"/", dirExtern))
    }else{
        system(paste0("rsync -r -a -e ssh ", dirLocal,"/ owos:/home/0/",idea.config.list$userDir ,"/", dirExtern))
    }
}

#' Load experiment results into result list
#'
#' @param reg The registry from which the results shall be loaded
#'
#' @export
ideaLoadResultList <- function(reg, doDelete = T){
    if(is.null(reg)){
        stop()
    }

    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))
    dir <- get("file.dir",reg)
    dirLocal <- dir
    dirExtern <- substring(dir, 3, nchar(dir))

    if(doDelete){
        system(paste0("rsync -r -a --delete -e ssh owos:/home/0/",idea.config.list$userDir ,"/", dirExtern, "/ ",dirLocal))
    }else{
        system(paste0("rsync -r -a -e ssh owos:/home/0/",idea.config.list$userDir ,"/", dirExtern, "/ ",dirLocal))
    }

    reduceResultsList(reg = reg)
}

#' Title
#'
#' @return
#' @export
#'
#' @import batchtools
ideaMakeRegistry <- function(mainDir, subDir, useCluster = T, ...) {
    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))
    dir.create(paste0(idea.config.list$desiredDir,"/Experiments"), showWarnings = FALSE)
    dir.create(paste0(idea.config.list$desiredDir,"/Experiments/",mainDir), showWarnings = FALSE)
    if(useCluster){
        conf.file <- paste0(idea.config.list$desiredDir,"/slurm.conf.R")
    }else{
        conf.file <- NULL
    }
    additionalParameters <- list(...)
    additionalParameters$file.dir <- paste0(idea.config.list$desiredDir,"/Experiments/",mainDir,"/",subDir)
    additionalParameters$conf.file <- conf.file
    additionalParameters$work.dir <- idea.config.list$desiredDir

    reg <- NULL
    reg <- try(batchtools::loadRegistry(file.dir = paste0(idea.config.list$desiredDir,"/Experiments/",mainDir,"/",subDir),writeable = T))
    if(is.null(reg) || is.error(reg)){
        do.call(batchtools::makeExperimentRegistry, args = additionalParameters)
    }
}

#' Title
#'
#' @return
#' @export
#'
#' @import batchtools
#' @import ssh
ideaSubmitJobs <- function(reg, ...){

    if(is.null(reg)){
        stop()
    }

    params <- list(...)
    synchronizeFolder()

    ### Make use of params!
    ###
    ###
    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))
    nodeName <- idea.config.list$nodeName
    sshInfo <- readSSHInfo(nodeName)
    sshKey <- sshInfo$identFile

    sess <- ssh::ssh_connect(host = paste0(sshInfo$user,"@",sshInfo$hostName),keyfile = sshInfo$identFile)

    ssh::ssh_exec_wait(session = sess, paste0("PATH=/opt/software/R/R-current/bin:$PATH ; /opt/software/R/R-current/bin/Rscript ",
                                              idea.config.list$desiredDir,"/submitJobs.R ",
                                              get("file.dir",envir = reg), " ",
                                              idea.config.list$desiredDir))
}

#' Title
#'
#' @return
#' @export
#'
#' @import batchtools
#' @import ssh
updatedPackage <- function(path){
    if(!endsWith(path,"/")){
        path <- paste0(path,"/")
    }

    # Copy configuration file back into package install dir
    file.copy(paste0(path, "config.rda"), system.file(package = "ideaBatch"))
    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))

    sshInfo <- readSSHInfo(idea.config.list$nodeName)
    sshKey <- sshInfo$identFile

    sess <- ssh::ssh_connect(host = paste0(sshInfo$user,"@",sshInfo$hostName),keyfile = sshInfo$identFile)

    ## Update Base Files
    dir <- idea.config.list$desiredDir
    dirLocal <- dir
    dirExtern <- substring(dir, 3, nchar(dir))
    if(!ideaPathIsBaseDir(paste0("/home/0/",idea.config.list$userDir ,"/", dirExtern))){
        stop("update Package tried to access a path which is note a base directory!")
    }
    system(paste0("rsync -d --delete -e ssh ", dirLocal,"/ owos:/home/0/",idea.config.list$userDir ,"/", dirExtern))

    ## Update Package Installation on Cluster
    ssh::ssh_exec_wait(session = sess, paste0("/opt/software/R/R-current/bin/Rscript ", idea.config.list$desiredDir,"/packageInstaller.R"))
}

ideaPathIsBaseDir <- function(path, sess = NULL){
    if(is.null(sess)){
        load(paste0(system.file(package = "ideaBatch"),"/config.rda"))

        sshInfo <- readSSHInfo(idea.config.list$nodeName)
        sshKey <- sshInfo$identFile

        sess <- ssh::ssh_connect(host = paste0(sshInfo$user,"@",sshInfo$hostName),keyfile = sshInfo$identFile)
    }
    files <- capture.output(ssh::ssh_exec_wait(session = sess, paste0("ls ", path)))

    shouldBeThere <- c("Experiments", "Sources", "packageInstaller.R", "sources.R")
    for(file in shouldBeThere){
        if(!(file %in% files)){
            return(F)
        }
    }
    return(T)
}
