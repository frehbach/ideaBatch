
#' Initiate ideaBatch package
#'
#' Initiate the ideaBatch package, install all requirements on the cluster copy files,
#' create your user config.
#'
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

    sshInfo <- readSSHInfo(nodeName)
    sshKey <- sshInfo$identFile

    sess <- ssh::ssh_connect(host = paste0(sshInfo$user,"@",sshInfo$hostName),keyfile = sshInfo$identFile)
    userDir <- rawToChar(ssh::ssh_exec_internal(session = sess, "pwd")$stdout)
    userDir <- strsplit(userDir,"/")[[1]][4]
    userDir <- substring(userDir,first = 1, last = nchar(userDir) - 1)

    idea.config.list <- list("userName" = userName, "sshKey" = sshKey, "nodeName" = nodeName,
                             "desiredDir" = desiredDir, "userDir" = userDir)
    save(idea.config.list,file = paste0(system.file(package = "ideaBatch"),"/config.rda"))

    prepareBaseFiles()
    dirExtern <- substring(desiredDir, 3, nchar(desiredDir))
    ssh::ssh_exec_wait(session = sess, paste0("mkdir -p /home/0/",userDir ,"/", dirExtern))
    synchronizeFolder()

    ssh::ssh_exec_wait(session = sess, paste0("/opt/software/R/R-current/bin/Rscript ", desiredDir,"/packageInstaller.R"))
}

#' Insert Variable into file
#'
#' Scans thorugh a file looking for codes in the form of e.g. #1# or #2# etc.
#' Parameters are filled from paramVector into the file
#'
#' @keywords internal
#'
#' @import batchtools
#' @import ssh
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

#' Parse ssh-config file
#'
#' Scans through the .ssh/config file looking for a configuration with the given nodeName
#'
#' @param nodeName string with the name to search for
#'
#' @return server info list
#'
#' @keywords internal
#'
#' @import batchtools
#' @import ssh
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
#' @param doDelete Whether or not the deletion option should be on in the rsync command
synchronizeFolder <- function(doDelete = T) {
    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))
    dir <- idea.config.list$desiredDir
    dirLocal <- dir
    dirExtern <- substring(dir, 3, nchar(dir))

    if(doDelete){
        system(paste0("rsync -r -a --delete -e ssh ", dirLocal,"/ ",idea.config.list$nodeName,":/home/0/",idea.config.list$userDir ,"/", dirExtern))
    }else{
        system(paste0("rsync -r -a -e ssh ", dirLocal,"/ ",idea.config.list$nodeName,":/home/0/",idea.config.list$userDir ,"/", dirExtern))
    }
}

#' Load experiment results into result list
#'
#' @param reg The registry from which the results shall be loaded
#' @param doDelete Whether or not the deletion option should be on in the rsync command
#' @param waitJobs Should the function wait for job completion? If False (default) it will not wait but might be missing
#' jobs which are still running
#'
#' @export
ideaLoadResultList <- function(reg, doDelete = T, waitJobs = F){
    if(is.null(reg)){
        stop()
    }

    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))
    dir <- get("file.dir",reg)
    dirLocal <- dir
    dirExtern <- substring(dir, 3, nchar(dir))

    while(T){
        if(!(get("cluster.functions", reg)$name == "Interactive")){
            if(doDelete){
                system(paste0("rsync -r -a --delete -e ssh ",idea.config.list$nodeName,
                              ":/home/0/",idea.config.list$userDir ,"/", dirExtern, "/ ",dirLocal))
            }else{
                system(paste0("rsync -r -a -e ssh ",idea.config.list$nodeName,
                              ":/home/0/",idea.config.list$userDir ,"/", dirExtern, "/ ",dirLocal))
            }
        }
        if(waitJobs){
            if(length(unlist(findDone())) == length(unlist(findExperiments()))){
                break
            }else{
                Sys.sleep(0.5)
            }
        }else{
            break
        }
    }

    reduceResultsList(reg = reg)
}

#' Create or load an IDE+A structured Registry
#'
#' Wrapper for batchtools::makeRegistry
#' If a registry in the desired directory already exists, it is loaded.
#'
#' @param mainDir The main directy for your registry to be stored in. E.g. if you are
#' doing experiments for a paper it might be the name of the paper your testing for: 'rehb18c'
#' @param subDir Some dir name that is stored in mainDir. E.g. "Test1", "KrigingOnESP"...
#' @param useCluster boolean, defaults to True shows wether the cluster shall be used or
#' if you want to run the experiments locally on your own laptop
#' @param ... Additional parameters passed to batchtools::makeRegistry
#'
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
        conf.file <- paste0(idea.config.list$desiredDir,"/batchtools.conf.R")
    }
    additionalParameters <- list(...)
    additionalParameters$file.dir <- paste0(idea.config.list$desiredDir,"/Experiments/",mainDir,"/",subDir)
    additionalParameters$conf.file <- conf.file
    additionalParameters$work.dir <- idea.config.list$desiredDir

    reg <- NULL
    reg <- try(batchtools::loadRegistry(file.dir = paste0(
        idea.config.list$desiredDir,"/Experiments/",mainDir,"/",subDir),writeable = T),silent = T)
    if(is.null(reg) || is.error(reg)){
        do.call(batchtools::makeExperimentRegistry, args = additionalParameters)
    }
}

#' Submit Jobs
#'
#' A wrapper around batchtools::submitJobs.
#' The wrapper is necessary to implement rsync usage to synchronize local files with the cluster.
#'
#' @param reg An IDE+A Registry
#' @param ... Additional parameters passed to batchtools::submitJobs
#'
#' @export
#'
#' @import batchtools
#' @import ssh
ideaSubmitJobs <- function(reg, ...){

    if(is.null(reg)){
        stop()
    }

    save.image(file = paste0(reg$file.dir,"/img.rda"))

    params <- list(...)
    synchronizeFolder()

    ### Make use of params!
    ###
    ###

    if(get("cluster.functions", reg)$name == "Interactive"){
        additionalParameters <- list(...)
        do.call(batchtools::submitJobs, args = additionalParameters)
    }else{
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
}


#' prepareBaseFiles
#'
#' Copies all required files from the package installation folder to the user specified foder
prepareBaseFiles <- function(){
    load(paste0(system.file(package = "ideaBatch"),"/config.rda"))
    desiredDir <- idea.config.list$desiredDir
    userDir <- idea.config.list$userDir

    dir.create(desiredDir, showWarnings = FALSE)
    file.copy(system.file("", "slurm.tmpl", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "slurm.conf.R", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "batchtools.conf.R", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "packageInstaller.R", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "sources.R", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "submitJobs.R", package = "ideaBatch"), desiredDir)
    file.copy(system.file("", "Sources/", package = "ideaBatch"), desiredDir, recursive = T)
    dir.create(paste0(desiredDir ,"/Experiments"), showWarnings = FALSE)


    file.copy(system.file("", "config.rda", package = "ideaBatch"), desiredDir)

    dirExtern <- substring(desiredDir, 3, nchar(desiredDir))
    insertPathToTemplate(paste0(desiredDir,"/slurm.tmpl"), c(userDir,dirExtern))
    insertPathToTemplate(paste0(desiredDir,"/slurm.conf.R"), c(desiredDir))
    insertPathToTemplate(paste0(desiredDir,"/batchtools.conf.R"), c(desiredDir))
    insertPathToTemplate(paste0(desiredDir,"/sources.R"), c(desiredDir))
}

#' UpdatedPackage
#'
#' This method should be called each time you install an update of the 'ideaBatch'-package.
#' Without this functions updates will break your package! Your config goes missing and the cluster runs
#' different files and versions than you do locally!!
#'
#' @param path The path to your main Project folder
#'
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
    prepareBaseFiles()

    dir <- idea.config.list$desiredDir
    dirLocal <- dir
    dirExtern <- substring(dir, 3, nchar(dir))
    if(!ideaPathIsBaseDir(paste0("/home/0/",idea.config.list$userDir ,"/", dirExtern))){
        stop("update Package tried to access a path which is note a base directory!")
    }
    system(paste0("rsync -d --delete -e ssh ", dirLocal,"/ ",idea.config.list$nodeName,":/home/0/",idea.config.list$userDir ,"/", dirExtern))

    ## Update Package Installation on Cluster
    ssh::ssh_exec_wait(session = sess, paste0("/opt/software/R/R-current/bin/Rscript ", idea.config.list$desiredDir,"/packageInstaller.R"))
}

#' ideaPathIsBaseDir
#'
#' Check if a path is really a registry dear before you delete it etc.
#'
#' @param path path to the folder
#' @param sess SSH session
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

#' Run or list ideaTutorials
#'
#' There are some tutorials for the usage of the ideaBatch and batchtools package available.
#' You can list them by simply typing 'ideaTutorial()'. You will get a list of available tutorials.
#' By specifying the tutorial index you can open a document: e.g. 'ideaTutorial(1)'
#'
#' @param index Integer, index of which tutorial to run. If NULL (default) all tutorials will be listed.
#'
#' @export
#'
#' @import batchtools
#' @import ssh
ideaTutorial <- function(index = NULL) {
    tutorialFiles <- dir(system.file("Tutorials",package="ideaBatch"))
    tutorials <- tutorialFiles[grep(pattern = ".html", tutorialFiles)]

    if(is.null(index)){
        print(tutorials)
    }else{
        if(!is.numeric(index)){
            stop("Index was not numeric")
        }
        indexes <- as.numeric(unlist(regmatches(tutorials, gregexpr("[[:digit:]]+", tutorials))))
        if(!(index %in% indexes)){
            stop("This tutorial does not exist")
        }
        browseURL(paste0(system.file("Tutorials",package="ideaBatch"), "/", tutorials[which(indexes == index)]))
    }
}
