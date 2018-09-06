
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

    synchronizeFolder()

    ssh::ssh_exec_wait(session = sess, paste0("/opt/software/R/R-current/bin/Rscript ", desiredDir,"/packageInstaller.R"))
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
synchronizeFolder <- function() {
    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))
    dir <- idea.config.list$desiredDir
    dirLocal <- dir
    dirExtern <- substring(dir, 3, nchar(dir))

    #Problem remaining!!!!!!!
    #######
    ##########
    ##########
    system(paste0("rsync -r -a -v -e ssh --delete ", dirLocal," owos:/home/0/",idea.config.list$userDir ,"/"))
}

#' Synchronize Experiment Folder
#'
#' Synchronizes the Experiment folder to the cluster
#'
#' @param dir the dir that is synchronized
synchronizeJobsFolder <- function(jc){
    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))
    dir <- get("file.dir",jc)
    dirLocal <- paste0(dir, "/jobs")
    dirExtern <- substring(dir, 3, nchar(dir))

    #Problem remaining!!!!!!!
    #######
    ##########
    ##########
    system(paste0("rsync -r -a -v -e ssh --delete ", dirLocal," owos:/home/0/",idea.config.list$userDir ,"/", dirExtern))
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
        stop("no local conf file supplied!")
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
ideaSubmitJobs <- function(...){
    ####
    ####
    #### Has To take more params!!!
    ####
    params <- list(...)
    synchronizeFolder()
    do.call(submitJobs, args = params)
}
