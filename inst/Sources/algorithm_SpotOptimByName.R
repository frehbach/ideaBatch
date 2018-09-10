#' Title
#'
#' @param strAlgoName
#' @param control
#' @param ...
#'
#' @return
#' @export
#'
#' @import SPOT
algorithm_SpotOptimByName <- function(strAlgoName, lower, upper, control, ...){
    algFun <- function(job, data, instance) {
        optimAlgo <- getAnywhere(strAlgoName)[1]
        result <- optimAlgo(x = NULL, fun = instance,lower = lower, upper = upper, control = control,...)
        result$job <- job
        result$clusterAnalysis$iterResults <- result$y
        result
    }
    return(algFun)
}
