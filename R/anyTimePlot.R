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

# algoName ; replication ; iteration ; iterBest
anyTimePerformancePlot <- function(data, confidenceInterval = 0.95, yLim = NULL, xLim = NULL, ylog = F, xlog = F){
    require(ggplot2)

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

            error <- qnorm(1-(1-confidenceInterval)/2)*sdVal/sqrt(n)
            min <- meanVal - error
            max <- meanVal + error
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

    h <- ggplot(plotDF, aes(x = iteration, y = mean, color = algoName)) + geom_line()
    h <- h + geom_ribbon(aes(ymin = min, ymax = max, fill = algoName), alpha = 0.2, show.legend = F)
    if(ylog){
        h <- h + scale_y_log10()
    }
    if(xlog){
        h <- h + scale_x_log10()
    }
    h <- h + coord_cartesian(ylim = yLim, xlim = xLim)
    h <- h + scale_colour_manual(name = "Algorithm:",values=c("red","blue","black"))
    h
}
