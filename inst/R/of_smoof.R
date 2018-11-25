#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#' @export
#'
#' @import smoof
of_smoofByName <- function(strName, nDim = 2){
    createdFun <- makeFunctionsByName(strName, dimensions = nDim)[[1]]
    smoofFun <- function(candidate){
        if(typeof(candidate) == "list"){
            return(unlist(lapply(candidate,createdFun)))
        }else if(length(candidate) > length(attr(createdFun,"par.set")$pars$x$lower)){
            candidate <- matrix(candidate,ncol = length(attr(createdFun,"par.set")$pars$x$lower), byrow=T)
            return(unlist(apply(candidate,1,createdFun)))
        }else{
            return(createdFun(candidate))
        }
    }
    return(smoofFun)
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
getAllSmoofFunctions <- function(){
    smoofNames <- smoof::filterFunctionsByTags(tags = "single-objective")
    return(smoofNames)
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
getSmoofFunLowerBounds <- function(strName){
    fun <- getSmoofFunByName(strName)
    return(smoof::getLowerBoxConstraints(fun))
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
getSmoofFunUpperBounds <- function(strName){
    fun <- getSmoofFunByName(strName)
    return(smoof::getUpperBoxConstraints(fun))
}

#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#'
#' @keywords internal
checkSmoofFunScalibility <- function(strName){
    functionBlackList <- c("Shekel function","Hartmann")
    if(strName %in% functionBlackList){
        return(FALSE)
    }
    return(strName %in% smoof::filterFunctionsByTags("scalable"))
}
