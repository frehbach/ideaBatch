#' Title
#'
#' @return
#' @export
makeClusterFunctionsIDEASlurm <- function (template = "slurm", array.jobs = TRUE, nodename = "localhost",
          scheduler.latency = 1, fs.latency = 65)
{
    assertFlag(array.jobs)
    assertString(nodename)
    template = findTemplateFile(template)
    if (testScalarNA(template))
        stopf("Argument 'template' (=\"%s\") must point to a readable template file",
              template)
    template = cfReadBrewTemplate(template, "##")
    quote = if (isLocalHost(nodename))
        identity
    else shQuote
    getClusters = function(reg) {
        clusters = filterNull(lapply(reg$resources$resources,
                                     "[[", "cluster"))
        if (length(clusters))
            return(stri_flatten(unique(as.character(clusters)),
                                ","))
        return(character(0L))
    }
    submitJob = function(reg, jc) {
        assertRegistry(reg, writeable = TRUE)
        assertClass(jc, "JobCollection")
        if (jc$array.jobs) {
            logs = sprintf("%s_%i", fs::path_file(jc$log.file),
                           seq_row(jc$jobs))
            jc$log.file = stri_join(jc$log.file, "_%a")
        }
        outfile = cfBrewTemplate(reg, template, jc)

        ## IDEA Synchronize
        synchronizeFolder()
        res = runOSCommand("sbatch", outfile, nodename = nodename)
        output = stri_flatten(stri_trim_both(res$output), "\n")
        if (res$exit.code > 0L) {
            temp.errors = c("Batch job submission failed: Job violates accounting policy (job submit limit, user's size and/or time limits)",
                            "Socket timed out on send/recv operation", "Submission rate too high, suggest using job arrays")
            i = wf(stri_detect_fixed(output, temp.errors))
            if (length(i) == 1L)
                return(makeSubmitJobResult(status = i, batch.id = NA_character_,
                                           msg = temp.errors[i]))
            return(cfHandleUnknownSubmitError("sbatch", res$exit.code,
                                              res$output))
        }
        id = stri_split_fixed(output[1L], " ")[[1L]][4L]
        if (jc$array.jobs) {
            if (!array.jobs)
                stop("Array jobs not supported by cluster function")
            makeSubmitJobResult(status = 0L, batch.id = sprintf("%s_%i",
                                                                id, seq_row(jc$jobs)), log.file = logs)
        }
        else {
            makeSubmitJobResult(status = 0L, batch.id = id)
        }
    }
    listJobs = function(reg, args) {
        assertRegistry(reg, writeable = FALSE)
        args = c(args, "--noheader", "--format=%i")
        if (array.jobs)
            args = c(args, "-r")
        clusters = getClusters(reg)
        if (length(clusters))
            args = c(args, sprintf("--clusters=%s", clusters))
        res = runOSCommand("squeue", args, nodename = nodename)
        if (res$exit.code > 0L)
            OSError("Listing of jobs failed", res)
        if (length(clusters))
            tail(res$output, -1L)
        else res$output
    }
    listJobsQueued = function(reg) {
        args = c(quote("--user=$USER"), "--states=PD")
        listJobs(reg, args)
    }
    listJobsRunning = function(reg) {
        args = c(quote("--user=$USER"), "--states=R,S,CG")
        listJobs(reg, args)
    }
    killJob = function(reg, batch.id) {
        assertRegistry(reg, writeable = TRUE)
        assertString(batch.id)
        cfKillJob(reg, "scancel", c(sprintf("--clusters=%s",
                                            getClusters(reg)), batch.id), nodename = nodename)
    }
    makeClusterFunctions(name = "Slurm", submitJob = submitJob,
                         killJob = killJob, listJobsRunning = listJobsRunning,
                         listJobsQueued = listJobsQueued, array.var = "SLURM_ARRAY_TASK_ID",
                         store.job.collection = TRUE, store.job.files = !isLocalHost(nodename),
                         scheduler.latency = scheduler.latency, fs.latency = fs.latency)
}
