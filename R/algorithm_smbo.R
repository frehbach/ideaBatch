#' Title
#'
#' @param strName
#' @param nDim
#'
#' @return
#' @export
#'
#' @import CEGO
algorithm_SMBO <- function(modelOptimizer,
                           distanceFunction, designGenerator,
                           budget = 100,
                           useBP = F, useEI = F){
    smbo <- function(job, data, instance){
        evalFun <- instance

        X <- designGenerator()
        y <- evalFun(X)

        pointsPerIter <- sum(useBP, useEI)
        if(pointsPerIter == 0){
            stop("Cant run SMBO without infill criterion")
        }
        while((length(y) + pointsPerIter) <= budget){
            lX <- tapply(X,rep(1:nrow(X),ncol(X)),function(i)i)

            naInd <- !is.na(y)

            modelY <- y[naInd]
            modelX <- lX[naInd]
            surrogateModel <- CEGO::modelKriging(modelX, modelY, distanceFunction,
                             control = list(reinterpolate = T,
                                            useLambda = T,
                                            combineDistances = T))
            surrogateModel$predAll = T

            evalY = function(x){return(predict(surrogateModel,x)$y)}
            evalS = function(x){
                yVal = evalY(x)
                yMin = min(y, na.rm = T)
                s = predict(surrogateModel,x)$s
                EITermOne = (yMin - yVal) * pnorm((yMin - yVal)/s)
                EITermTwo = s * (1/sqrt(2 * pi)) * exp(-(1/2) * ((yMin -
                                                                      yVal)^2/(s^2)))
                return(-log10(EITermOne + EITermTwo + (.Machine$double.xmin)))
            }

            infillCriteria <- NULL
            if(useBP) infillCriteria <- c(infillCriteria, evalY)
            if(useEI) infillCriteria <- c(infillCriteria, evalS)

            modelOptima <- matrix(NA,nrow = length(infillCriteria),ncol = ncol(X))
            X <- rbind(X, modelOptima)
            y <- c(y, evalFun(modelOptima))
        }
        result <- list("X" = X, "y" = y, "job" = job)
        result$clusterAnalysis$iterResults <- y
        return(result)
    }
}

