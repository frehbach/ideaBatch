#' Compile the 'batchtools' result list into a data.frame
#'
#' Currently uses FR format for FR's plots, ask me ;-)
#'
#' @param resultList the list you get by ideaLoadResultList
#'
#' @return resultDF
#' @export
generateResultDF <- function(resultList){
    resultDF <- data.frame(algoName=character(),
                           problemName=character(),
                           replication=character(),
                           seed = integer(),
                           iteration = integer(),
                           iterBest = double(),
                           stringsAsFactors=FALSE)
    for(e in resultList){
        algoName <- e$job$algo.name
        problemName <- e$job$prob.name
        replication <- e$job$repl
        seed <- e$job$seed

        iterResults <- e$clusterAnalysis$iterResults
        iterBests <- unlist(lapply(1:length(iterResults),function(x)min(iterResults[1:x],na.rm = T)))
        for(i in 1:length(iterBests)){
            resultDF[nrow(resultDF)+1,] <- c(algoName,problemName,replication,seed,i, iterBests[i])
        }
    }
    resultDF$replication <- as.numeric(resultDF$replication)
    resultDF$iteration <- as.numeric(resultDF$iteration)
    resultDF$seed <- as.numeric(resultDF$seed)
    resultDF$iterBest <- as.numeric(resultDF$iterBest)
    resultDF
}

#' Create an anytimePerformancePlot
#'
#' @param data The data you wish to plot. (Should usually be only data of 1 objective function)
#' @param yLim Limits for y axis
#' @param xLim limits for x axis
#' @param ylog Logarithmic y axis?
#' @param xlog Logarithmic x axis?
#' @param useMinMax If T Min and max are plotted, otherwise confidenceintervals
#' @param confidenceInterval If useMinMax = F this sets the confidenceIntervals which are plotted
#'
#' @return A ggplot
#' @export
anyTimePerformancePlot <- function(data, yLim = NULL, xLim = NULL, ylog = F
                                   , xlog = F, useMinMax = T, confidenceInterval = 0.95){
    requireNamespace("ggplot2")
    dfNames <- c("algoName", "replication", "iteration", "iterBest")
    if(!all(dfNames %in% names(data))){
        stop("Wrong df names were provided")
    }

    #Build matrix : algoName; iteration; mean; min ; max
    plotDF <- NULL
    for(method in unique(data$algoName)){
        subData <- data[which(data$algoName == method),]
        for(i in sort(unique(subData$iteration))){
            meanVal <- mean(subData$iterBest[which(subData$iteration == i)])
            sdVal <- sd(subData$iterBest[which(subData$iteration == i)])
            n <- length(subData$iterBest[which(subData$iteration == i)])

            if(!useMinMax){
                error <- qnorm(1-(1-confidenceInterval)/2)*sdVal/sqrt(n)
                min <- meanVal - error
                max <- meanVal + error
            }else{
                min <- min(subData$iterBest[which(subData$iteration == i)])
                max <- max(subData$iterBest[which(subData$iteration == i)])
            }


            plotDF <- rbind(plotDF, c(
                method, i, meanVal,
                min,
                max
            ))
        }
    }
    plotDF <- as.data.frame(plotDF)
    colnames(plotDF) <- c("algoName","iteration","mean","min", "max")
    plotDF$iteration <- as.numeric(levels(plotDF$iteration))[plotDF$iteration]
    plotDF$mean <- as.numeric(levels(plotDF$mean))[plotDF$mean]
    plotDF$min <- as.numeric(levels(plotDF$min))[plotDF$min]
    plotDF$max <- as.numeric(levels(plotDF$max))[plotDF$max]

    h <- ggplot2::ggplot(plotDF, ggplot2::aes(x = iteration, y = mean, color = algoName)) + ggplot2::geom_line()
    h <- h + ggplot2::geom_ribbon(ggplot2::aes(ymin = min, ymax = max, fill = algoName), alpha = 0.2, show.legend = F)
    if(ylog){
        h <- h + ggplot2::scale_y_log10()
    }
    if(xlog){
        h <- h + ggplot2::scale_x_log10()
    }
    h <- h + ggplot2::coord_cartesian(ylim = yLim, xlim = xLim)
    h <- h + ggplot2::scale_colour_manual(name = "Algorithm:",values=c("red","blue","black", "green", "yellow"))
    h
}
