# library(ggplot2)
# library(fpc)
# library(lsa)
# library(extrafont)
# loadfonts(quiet=T)


k.med <- function(matrix, data, metric){
  # Args:
  #   matrix: the dtm matrix returned from process.text()
  #   data: the data.frame used in process.text()
  #   distance: the metric used in clustering 
  #       - euclidean
  #       - manhattan
  # Returns:
  #   returns a data frame of all cluster summary stats used to produce bubble plot
  # Additional work:
  #   add additional customization args, and plots
  pamResult <- pamk(matrix, metric = metric)
  # number of clusters identified
  (k <- pamResult$nc)
  
  pamResult <- pamResult$pamobject
  struct <- signif(pamResult$silinfo$clus.avg.widths,3)
  data$cluster <- pamResult$clustering

  data <- data %>%
    dplyr::group_by(cluster) %>%
    dplyr::arrange(desc(engage))
  
  means <- data %>% 
    dplyr::group_by(cluster) %>%
    dplyr::summarise(volume = n(),
              engagment = mean(engage, na.rm = T),
              total_reach = if("reach" %in% names(data)) sum(reach, na.rm = T) else NA,
              klout = if("influence" %in% names(data)) mean(influence, na.rm = T) else NA,
              sent = mean(sent, na.rm = T))
  means[is.na(means)] <- 1
  means$color <- ifelse (means$sent > 0, "#73BC4F",
                         ifelse (means$sent < 0, "#F54343", "#314050"))
  
  means$direction <- ifelse (means$color == "#73BC4F", 24,
                             ifelse (means$color == "#F54343", 25, 23))
  
  clus = 0
  for (i in 1:k){
    suppressWarnings(
      ifelse (sum(pamResult$medoids[i,]) > 0,
              clus[i] <- colnames(pamResult$medoids)[which(pamResult$medoids[i,] == max(pamResult$medoids[i,]))],
              clus[i] <- "Misc")
    )
  }
  names<-toupper(as.character(clus))
  
  for (i in 1:length(names)){
    print(sprintf("Cluster %i: %s -- Structure: %s", i, names[i], struct[i]))
  }
  means$names <- names
  
  data.Split <- split(data, data$cluster)
  return(list('plot_data' <- means, 'split_data' <- data.Split))
}



cluster <- function(matrix, data, algorithm, k){
  # Args:
  #   matrix: the dtm matrix returned from process.text()
  #   data: the data.frame used in process.text()
  #   algorithm: the algorithm to use for clustering
  #       - kmeanCBI
  #       - pamkCBI
  #   method: the method of sampling of the data frame
  #       - boot
  # Returns:
  #   returns a data frame of all cluster summary stats used to produce bubble plot
  # Additional work:
  #   add additional customization args & algorithms
  set.seed(123)
  bootstrap <- clusterboot(matrix,
                           bootmethod = "boot",
                           count = FALSE,
                           clustermethod = algorithm)
  bootstrap
  clus = 0
  data$cluster <- bootstrap$result$result$cluster
  
  data<- data %>%
    dplyr::group_by(cluster) %>%
    dplyr::arrange(desc(engage))
  
  means <- data %>% 
    dplyr::group_by(cluster) %>%
    dplyr::summarise(volume = n(),
                     engagment = mean(engage, na.rm = T),
                     total_reach = sum(reach, na.rm = T),
                     klout = mean(influence, na.rm = T),
                     sentiment = mean(sent, na.rm = T))
  means[is.na(means)] <- 1
  means$color <- ifelse (means$sentiment > 0, "#73BC4F",
                         ifelse (means$sentiment < 0, "#F54343", "#314050"))
  
  means$direction<-ifelse (means$color == "#73BC4F", 24,
                           ifelse (means$color == "#F54343", 25, 23))
  means <- means[-which(means$volume == max(means$volume)),]
  
  for (i in 1:bootstrap$nc){
    suppressWarnings(
      ifelse (sum(bootstrap$result$result$centers[i,]) > 0,
              clus[i] <- colnames(bootstrap$result$result$centers)[which(bootstrap$result$result$centers[i,] == max(bootstrap$result$result$centers[i,]))],
              clus[i] <- "Misc")
    )
  }
  names<-toupper(as.character(clus))
  for (i in 1:length(names)){
    print(sprintf("Cluster %i: %s", i, names[i]))
  }
  means$names <- names
  
  data.Split <- split(data, data$cluster)
  data.Split <- data.Split[-which(means$volume == max(means$volume))]
  names(data.Split) <- names
  
  return(list('plot_data' <- means,
              'split_data' <- data.Split))
}

rm.cluster <- function(clustered, cluster){
  # function to remove poorly structured clusters
  # Args:
  #   clustered: is the list of the two data types with the clustered data
  #   cluster: the cluster to remove from the data
  # Returns:
  #   clustered data with necessary content removes.
  clustered[[1]] <- clustered[[1]][-which(clustered[[1]][['cluster']] == cluster),]
  clustered[[2]] <- clustered[[2]][-which(as.numeric(as.character(names(clustered[[2]]))) == cluster)]
  
  return(clustered)
}


summ.clusters <- function(data, trunc = 10, myStopwords=c("")){
  contents <- lapply(data, function(x)as.character(x[['content']]))
  
  corpus <- lapply(contents,function(x)Corpus(VectorSource(x)))
  corpus <- lapply(corpus,function(x)tm_map(x, removePunctuation))
  corpus <- lapply(corpus,function(x)tm_map(x, removeNumbers))
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  corpus <- lapply(corpus,function(x)tm_map(x, removeURL))
  corpus <- lapply(corpus,function(x)tm_map(x, tolower))
  corpus <- lapply(corpus,function(x)tm_map(x, removeWords, myStopwords))
  corpus <- lapply(corpus,function(x)tm_map(x, stripWhitespace))
  corpus <- lapply(corpus,function(x)tm_map(x, PlainTextDocument))
  
  options(mc.cores = 1)
  BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))}
  mats <- lapply(corpus,function(x)TermDocumentMatrix(x[], control = list(tokenize = BigramTokenizer, 
                                                                        weighting = function(mat)weightTfIdf(mat, normalize=FALSE))))
  
  
  mats <- lapply(mats, function(x)as.matrix(x))
  svd <- lapply(mats, function(x)svd(x, nu = min(nrow(x), ncol(x)), nv = min(nrow(x),ncol(x))))
  
  salience <- lapply(svd, function(x)sqrt((x$d^2)*(t(x$v)^2)))
  salience <- lapply(salience, function(x) apply(x , 2, sum))
  salience <- lapply(salience, function(x) as.data.frame(sqrt(x)))
  salience <- mapply(cbind, salience,"text"= contents,SIMPLIFY=F)
  salience <- lapply(salience, function(x) x[ order(-x[,1]), ])
  clustSummary <- lapply(salience, function(x) as.character(x$text[1:trunc]))
  return(clustSummary)
}



plot.cluster <- function(clustered, bubble){
  visualData <- clustered
  visualData$sent <- visualData$sent/10
  ymin <- ifelse (min(visualData$sent) < 0,
                  (min(visualData$sent) - .25),
                  (-.25))
  
  ymax <- ifelse (max(visualData$sent) > 0,
                  (max(visualData$sent) + .25),
                  .25)
  
  ylowbreak <- ymin + .15
  ytopbreak <- ymax - .15
  xmin <- (-1)
  xmax <- (nrow(visualData) + 1)
  
  posneg <- ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .5, .05,
                  ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .4, .04,
                          ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .3, .03,
                                  ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .2, .02, .01))))
  
  
  visualData$startSample <- '>'
  visualData$endSample <- '//'
  visualData <- visualData[order(visualData$names), ]
  
  visualData$pointer <- rep(c(1,-1),length = nrow(visualData))
  
  visualData$line <- ifelse (visualData$pointer == 1, 
                           (ytopbreak),
                           (ylowbreak))
  
  visualData$counter <- as.numeric(as.factor(visualData$names))
  
  visualData$ymin.text <- ifelse(visualData$pointer == 1,
                               ytopbreak+.005,
                               ymin-.125)
  visualData$ymax.text <- ifelse(visualData$pointer == 1,
                               ymax+.105,
                               ylowbreak-.045)
  visualData$y.text <- ifelse(visualData$pointer == 1,
                            ymax + .135,
                            ylowbreak - .02)
  
  visualData$sizing <- visualData[[bubble]]
  
  bubblePlot <- ggplot(visualData, aes(names, sent, ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax)) + 
    
    geom_point(aes(size = sizing), colour = 'grey10',alpha=.5,
               stat = "identity", position = "identity") +
    
    geom_point(aes(size = sizing), colour = 'grey90',alpha=.15,
               stat = "identity", position = "identity") +
    
    annotate("text", x = -0.5 , y = posneg , label = "+", size=8, colour = 'grey70', family = 'DIN Condensed Bold') +
    
    annotate("text", x = -0.5, y = -posneg, label = "-", size=8, colour = 'grey70', family = 'DIN Condensed Bold') +
    
    annotate("segment", x = visualData$counter, xend = visualData$counter, y = visualData$sent, yend = visualData$line,
             colour = 'grey10')+
    
    annotate('text', x = (visualData$counter), y = (visualData$y.text), 
             label = visualData$names, colour='white', family = 'DIN Alternate Bold', hjust = 1, size = 7) +
    
    annotate('text', x = visualData$counter - 1.85, y = visualData$ymax.text, 
             label = visualData$startSample, colour = '#F54343', size = 5, family = 'DIN Alternate Bold') +
    
    annotate('text', x = visualData$counter + .045, y = visualData$ymin.text, 
             label = visualData$endSample, colour = '#F54343', size = 5, family = 'DIN Alternate Bold') +
    
    geom_hline(yintercept = 0,
               size = .5,
               colour = 'grey70') +
    
    geom_hline(yintercept = ytopbreak,
               size = .5,
               colour = 'grey70') +
    
    geom_hline(yintercept =  ylowbreak,
               size = .5,
               colour = 'grey70') +
    
    geom_point(aes(fill = factor(color)), alpha = .85, size = 10,
               stat = "identity", position = "identity", shape = 21) +
    
    geom_point(shape = visualData$direction,fill = 'white', alpha = .85, size = 3,
               stat = "identity", position = "identity") +
    
    
    scale_size(range = c(30, 70)) +
    
    theme(panel.background = element_rect(fill = "#314050",colour = NA),
          plot.background = element_rect(fill = "#314050",colour = NA),
          text=element_text(family = 'DIN Condensed Bold'),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          panel.grid  = element_blank()) +
    
    scale_fill_identity() +
    scale_color_identity()
  
  return(bubblePlot)
}
