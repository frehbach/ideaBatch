#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#' @export
#'
#' @import CEGO
algorithm_CegoByName <- function(strAlgoName,control,...){
    algFun <- function(job, data, instance) {
        optimAlgo <- getAnywhere(strAlgoName)
        result <- optimAlgo(fun = instance,control = control,...)
        result$job <- job
        result$clusterAnalysis$iterResults <- result$y
        result
    }
    return(algFun)
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
cego_CreationFunctionReal = function(nDim,lBounds = rep(-5,nDim), uBounds = rep(5,nDim))
{
    createReal <- function(){
        runif(nDim, lBounds, uBounds)
    }
    createReal
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
cego_MutationReal = function(population, parameters)
{
    mutationRate <- 0.05
    N <- length(population[[1]])
    popsize <- length(population)
    newpop <- list()
    for(i in 1:popsize)
    {
        individual <- population[[i]]
        ind = sample(length(individual),1)
        individual[ind] = round(individual[ind] * runif(1,1-(10*mutationRate),1 + (10*mutationRate)),4)
        newpop <- c(newpop, list(individual))
    }
    newpop
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
cego_RecombinationReal = function(population, parameters)
{
    N <- length(population[[1]])
    popsize <- length(population)/2
    newpop <- list()
    for (i in 1:popsize) {
        index <- sample(2:(N - 1), 1, FALSE, NULL)
        inds <- 1:index
        j <- popsize + i
        parent1 <- population[[i]]
        parent1[inds] <- population[[j]][inds]
        newpop <- c(newpop, list(parent1))
    }
    newpop
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
cego_DistanceReal = function(x, y)
{
    sum(abs(x - y))/max(length(x), length(y))
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
cego_DistanceManhatton = function (x, y)
{
    sum(abs(x - y))/max(length(x), length(y))
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
cego_CreationFunctionBinary <- function(nDim)
{
    creaFun <- function(){
        return(sample(0:1,nDim,replace = T))
    }
    creaFun
}

