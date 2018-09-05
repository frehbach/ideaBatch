#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#' @export
#'
#' @import cmaes
algorithm_CMAES <- function(lower,upper,control = list(),...){
    control$diag.value = T
    algFun <- function(job, data, instance) {
        result <- cma_es(par = cego_CreationFunctionReal(nDim = length(lower),
                                                         lBounds = lower,
                                                         uBounds = upper)(),
                         fn = instance,
                         ...,
                         lower = lower,
                         upper = upper,
                         control = control)
        result$job <- job
        result$clusterAnalysis$iterResults <- job$diagnostic$value[,1]
        result
    }
    return(algFun)
}
