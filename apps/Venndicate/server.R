
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

##### Load Packages #####
require(shiny)        # Main Shiny Package
require(ggplot2)      # Plotting Package
require(stringr)      # String manipulation
require(jsonlite)     # JSON manipulation
require(RWeka)        # R/Weka interface
require(tm)           # Text Mining Package
require(plyr)         # Tools for splitting, applying and combining data
require(dplyr)        # dplyr: a grammar of data manipulation
require(SnowballC)    # Snowball stemmers based on the C libstemmer UTF-8 library
require(fpc)          # Flexible procedures for clustering
require(lsa)          # Latent Semantic Analysis
require(extrafont)    # Tools for using fonts
require(igraph)       # Network analysis and visualization
require(reshape2)     # Flexibly reshape data: a reboot of the reshape package
require(wordcloud)    # Word Clouds
require(shinyIncubator)

##### Set Global Options #####
options(shiny.maxRequestSize=100*1024^2)
options(scipen = 999)
loadfonts(quiet=T)


##### Load Stopwords ######
myStopwords <- stopwords(getwd())


#####  ShinyServer  ######
shinyServer(function(input, output, session) {
   
  # Tests for a file upload
  output$fileUploaded <- reactive({
    if( is.null(input$data) ) TRUE else FALSE 
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  
  ###### Reactive Values ######
  raw <- reactiveValues()
  agg <- reactiveValues()
  siz <- reactiveValues()
  cls <- reactiveValues()
  
  
  ############### 1. Read and Agg ###############
  
  ###### Parse.JSON ######
  observe({
    
    if( !is.null(input$data) ) {
      withProgress(session, {
        setProgress(message="Parsing Data File", value=0, detail="Converting from JSON format" )
        raw$json <- parse.json(input$data[1,'datapath'], shiny=TRUE)
        raw$backup <- isolate({raw$json})
      })
    }
  })
  
  ##### Filter.rm/Filer.add #####
  observe({
    if( input$remove > 0 && input$filterText != "Text to filter") {
      raw$json <- isolate({ filter.rm(raw$json, filter = input$filterText ) })
    }else if( !is.null(input$remove) ){
      raw$json <- isolate({ filter.rm(raw$json, filter = "" ) })
    }
  })
  observe({
    if( input$add > 0 && input$filterText != "Text to filter") {
      raw$json <- isolate({ filter.add(raw$json, filter = input$filterText ) })
    }
  })
  
  ##### Reset Data #####
  observe({
    if( input$dataReset > 0 ){
      raw$json <- raw$backup
    }
  })
  
  ###### Split by Tags ######
  jsonByTag <- reactive({
      split <- split.tags(raw$json)
      return(split)
  })
  
  ##### Split by Source #####
  jsonByType <- reactive({
      if( !is.null(input$tagSelect) && input$tagSelect == "All" && !is.null(raw$json) ){
        split <- split.type(raw$json)
      }else if( !is.null(input$tagSelect) && input$tagSelect != "All" && isolate({ !is.null(jsonByTag()) }) ){
        split <- split.type(jsonByTag()[[input$tagSelect]])
      }
      return(split)
  })
  
  
  ##### Populate Tag Dropdown #####
  observe({
    if( !is.null(raw$json) ){
      updateSelectizeInput(session, 'tagSelect', choices=c("All", names(jsonByTag())), selected="All")
    }
  })
  
  
  ##### Aggregate Data #####
  observe({
    if( !is.null(raw$json) && !is.null(jsonByType()) ){
      withProgress(session, {
        
        setProgress(message="Aggregating Data", value=0)
        # Determine which types exist
        if( !is.null(jsonByType()) ){
          types = names(jsonByType())
        }
        
        # Aggregate Content
        setProgress(detail="Content", value=0.01)
        agg$content <- agg.content(raw$json)
        
        for(i in 1:length(types)){
          setProgress(detail=types[i], value=i/(length(types)+1) )
          
          # Aggregate Facebook data
          if("facebook" == types[i]) agg$facebook <- agg.facebook(jsonByType()[[i]])
          
          # Aggregate Tumblr Data
          if("tumblr" == types[i]) agg$tumblr <- agg.tumblr(jsonByType()[[i]])
          
          # Aggregate Twitter data
          if("twitter" == types[i]) agg$twitter <- agg.twitter(jsonByType()[[i]])
          
          # Aggregate Wordpress Data
          if("wordpress" == types[i]) agg$wordpress <- agg.wordpress(jsonByType()[[i]])
        }
        
        setProgress(detail="Wrapping up", value=1)
      })
    }
  })
  
  
  # Aggregating for top links and writing CSV to working directory
  links <- reactive({ 
      rank.links(raw$json) 
  })
  
  # Aggregating for top users and writing CSV to working directory
  users <- reactive({ 
      rank.users(raw$json) 
  })
  
  
  
  
  ############### Download Handlers ###############
  # Link
  output$links <- downloadHandler(
    filename = function() { 
      name = strsplit(input$data[1,'name'], ".json")[[1]][1]
      paste0(name, "-links.csv")
    },
    content = function(file){
      write.csv(links(), file) 
    }
    )
  
  # Users
  output$users <- downloadHandler(
    filename = function() {
      name = strsplit(input$data[1,'name'], ".json")[[1]][1]
      paste0(name, "-users.csv")
    },
    content = function(file){
      write.csv(users(), file) 
    }
  )
  
  
  
  ############### Stats Tables ###############
  observe({
    if( !is.null(raw$json) && !is.null(agg$content) ){
      withProgress(session,{
        setProgress(message="Calculating Sizes", value=0.01)
        
        # Create All Stat
        x = unlist(mapply(function(s){nchar(as.character(s))}, agg$content$content, SIMPLIFY=F, USE.NAMES=F))
        t = data.frame(length = x)
        siz$all = t
        
        setProgress(value=0.2)
        
        # Create Tag Stat
        tags = names(jsonByTag())
        lens = mapply(function(x){ length(x) }, jsonByTag(), SIMPLIFY=T, USE.NAMES=F)
        t = data.frame(N=lens)
        row.names(t) = tags
        siz$tags = t
        
        setProgress(value=0.4)
        
        # Create Type Stat
        types = names(jsonByType())
        lens = mapply(function(x){ length(x) }, jsonByType(), SIMPLIFY=T, USE.NAMES=F)
        t = data.frame(N=lens)
        row.names(t) = types
        siz$types = t
        
        setProgress(value=0.6)
        
        #Create Agg Stat
        types = names(jsonByType())
        lens = mapply(function(t){
          x = eval(parse(text=paste0("agg$", t)))
          length(x[[1]]) 
        }, types, SIMPLIFY=T, USE.NAMES=F)
        lens = c(lens, length(agg$content[[1]]))
        t = data.frame(N=lens)
        row.names(t) = c(types, "content")
        siz$agg = t
        
        setProgress(value=0.8)
        
        # Creat Rank Stat
        lens = c( length(users()[[1]]), length(links()[[1]]) )
        t = data.frame(N=lens)
        row.names(t) = c("users", "links")
        siz$rank = t
        
        setProgress(value=1)

      })
    }
  })
  
  
  ##### Overall Stats #####
  output$hist = renderPlot({
    if( !is.null(raw$json) && !is.null(siz$all) ){
      t=log10(siz$all)
      counts = hist(t$length, breaks=length(t$length)/5)$count
      breaks = hist(t$length, breaks=length(t$length)/5)$mids
      h = data.frame(counts=counts, breaks=breaks)
      m <-  ggplot(h, aes(x=breaks, y=counts))+
        geom_bar(stat='identity', colour='#FAFAD2', fill='#FAFAD2')+
        ggplot2::annotate('segment', x=median(t$length), xend=median(t$length),
                          y=0, yend=max(h$counts), colour='grey50', size=1.5)+
        ggplot2::annotate('text', x=median(t$length)-0.1, y=max(h$counts), 
                          label=paste0("Median\n", round(10^(median(t$length)), digits=0)), colour='grey50', 
                          family = 'DIN Alternate Bold', size=8, hjust=1, vjust=1)+
        xlab("Number of Characters [log10]")+
        theme(panel.background = element_rect(fill = "#314050",colour = NA),
              plot.background = element_rect(fill = "#314050",colour = NA),
              text = element_text(family = 'DIN Condensed Bold',size=20),
              axis.title.y = element_blank(),
              axis.title.x = element_text(size = 18, colour='grey50'),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 18, vjust = .5),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")
      print(m)
    }
  })
  
  ##### Split Tags Stats #####
  output$tagList = renderTable({
    if( !is.null(raw$json) && !is.null(siz$tags) ){
      t = siz$tags
    }else{
      t = as.data.frame(matrix(NA, nrow=1, ncol=1))
      colnames(t) = "N"
      row.names(t) = " "
    }
    return(t)
  }, display=c("s", "d"))
  
  ##### Split Type Stats #####
  output$typeList = renderTable({
    if( !is.null(raw$json) && !is.null(siz$types) ){
      t = siz$types
    }else{
      t = as.data.frame(matrix(NA, nrow=1, ncol=1))
      colnames(t) = "N"
      row.names(t) = " "
    }
    return(t)
  }, display=c("s", "d"))
  
  ##### Agg Stats #####
  output$aggStat = renderTable({
    
    if( !is.null(raw$json) && !is.null(agg$content) && !is.null(siz$agg) ){
      t = siz$agg
    }else{
      t = as.data.frame(matrix(NA, nrow=1, ncol=1))
      colnames(t) = "N"
      row.names(t) = " "
    }
    return(t)
  }, display=c("s", "d"))
  
  ##### Rank Stats #####
  output$rankStat = renderTable({
    
    if( !is.null(raw$json) && !is.null(siz$rank) ){
      t = siz$rank
    }else{
      t = as.data.frame(matrix(NA, nrow=1, ncol=1))
      colnames(t) = "N"
      row.names(t) = " "
    }
    return(t)
  }, display=c("s", "d"))
  
  
  
  
  
  
  ############### 2. Prepare & Explore ###############
  ##### Update typeSelect #####
  observe({
    if( !is.null(raw$json) ){
      updateSelectizeInput(session, 'typeSelect', choices=names(jsonByType()) )
    }
  })
  
  ##### Process addList #####
  addWords <- reactive({
    if( !is.null(input$addList) && input$addList != "Default value" ) {
      gsub("^\\s+|\\s+$", "", strsplit(input$addList, ",")[[1]])
    }
  })  

  
  ##### Process Text #####
   processed <- reactive({
    if( !is.null(input$typeSelect) && !is.null(input$process) && !is.null(raw$json)
        && input$typeSelect != "" && !is.null(agg$content)){
      t = eval(parse(text=paste0("agg$", input$typeSelect)))
  
      withProgress(session, {
        setProgress(message="Processing Text")
        isolate({
          moreWords = addWords()
          return(process.text(t, input$sparse, input$sample, input$n, myStopwords, moreWords))
          })
      })
    }
  })


   ##### Reset #####
   observe({
     if( input$processReset > 0 ){
       updateSliderInput(session, 'sparse', value=0.9)
       updateNumericInput(session, 'sample', value=1000)
       updateSliderInput(session, 'n', value=3)
       updateTextInput(session, 'addList', value="Default value")
     }
   })
  
  
  
  ##### Heatmap #####
  output$heatmap <- renderPlot({
    if( !is.null(processed()) && dim(processed()$dtm)[1] > 0 ){
      tryCatch(heat.map(processed()$dtm),
               error = function(e) print(paste0("Heatmap failed with error: ", e))   )
    }
  }, bg = "#314050")
  
  
  ##### Term Network #####
  output$termNetwork <- renderPlot({
    if( !is.null(processed()) && dim(processed()$adj_mat)[1] > 0 ){
      tryCatch(term.network(processed()$adj_mat),
               error = function(e) print(paste0("Term Network failed with error: ", e))   )
    }
  }, bg = "#314050")
  
  
  ##### Word Cloud #####
  output$wordCloud <- renderPlot({
    if( !is.null(processed()) && dim(processed()$adj_mat)[1] > 0 ){
      tryCatch(word.cloud(processed()$dtm),
               error = function(e) print(paste0("Word Cloud failed with error: ", e))   )
    }
  }, bg = "#314050")
  
  
  
  ##### Download Handlers #####
  # Heatmap #
  output$getHeatmap <- downloadHandler(
    filename = function() { 
      name = strsplit(input$data[1,'name'], ".json")[[1]][1]
      paste0(name, "-Heatmap.png")
    },
    content = function(file){
      png(file)
      print(heat.map(processed()$dtm))
      dev.off()
    },
    contentType = 'image/png'
  )
  
  # Term Network #
  output$getNetwork <- downloadHandler(
    filename = function() { 
      name = strsplit(input$data[1,'name'], ".json")[[1]][1]
      paste0(name, "-TermNetwork.png")
    },
    content = function(file){
      png(file)
      print(term.network(processed()$adj_mat))
      dev.off()
    },
    contentType = 'image/png'
  )
  
  # Term Network #
  output$getWordCloud <- downloadHandler(
    filename = function() { 
      name = strsplit(input$data[1,'name'], ".json")[[1]][1]
      paste0(name, "-WordCloud.png")
    },
    content = function(file){
      png(file)
      word.cloud(processed()$dtm)
      dev.off()
    },
    contentType = 'image/png'
  )
  
  
  
  ############### 3. Cluster & Plot ###############
  
  ##### Cluster #####
  observe({
    if( !is.null(input$algo) && !is.null(processed()) && !is.null(input$resetPlot) ){
      a = as.integer(input$algo)
      if(a != 1L) a = 1  ### <-- Remove this line to enable alternative clustering methods
      if( dim(processed()$dtm)[1] > 1 ){
        clust = switch(a,
             k.med(processed()$dtm, processed()$data, metric = "euclidean"),
             cluster(processed()$dtm, processed()$dtm, algorithm = pamkCBI, k = input$k),
             cluster(processed()$dtm, processed()$dtm, algorithm = kmeansCBI, k = input$k)
             )
        cls$plot_data = clust[[1]] 
        cls$split_data = clust[[2]]
      }
    }
  })
  
  
  ##### Populate Cluster Select #####
  observe({
    if( !is.null(cls$plot_data) && !is.null(input$rmCluster) ){
     names = paste0(cls$plot_data$cluster, " - ", cls$plot_data$names, " [", format(cls$plot_data$sent, width=2, digits=2), "]")
     n = as.list(cls$plot_data$cluster)
     names(n) = names
     
     updateSelectizeInput(session, 'clSelect', choices=n)
    }
  })
  
  
##### Cluser Plot #####
  output$clustPlot <- renderPlot({
      if( !is.null(cls$plot_data) && !is.null(summary()) ){
        visualData <- cls$plot_data
        examples = summary()
        bubble = switch(input$sizing,
                        'Reach'='total_reach',
                        'Volume'='volume')
        
        visualData$sent <- visualData$sent/10
        yMin <- ifelse (min(visualData$sent) < 0,
                        (min(visualData$sent) - .25),
                        (-.25))
        
        yMax <- ifelse (max(visualData$sent) > 0,
                        (max(visualData$sent) + .25),
                        .25)
        
        ylowbreak <- yMin + .15
        ytopbreak <- yMax - .15
        
        posneg <- ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .5, .05,
                         ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .4, .04,
                                ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .3, .03,
                                       ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .2, .02, .01))))
        
        
        visualData$startSample <- '>'
        visualData$endSample <- '//'
        
        
#         # format text samples
#         if( input$showSamples ){
#           visualData$sample = unlist(lapply(names(examples), function(s, n){ 
#             term = tolower(visualData[which(visualData['cluster'] == n ), 'names'])
#             
#             # take first example
#             t = s[[n]][1]
#             # split by term
#             split.t = strsplit(tolower(t), tolower(term))[[1]]
#             # split by whitespace
#             split.t = strsplit(split.t, "[[:space:]]")
#             # find punctuation
#             punc1 = grep("[[:alpha:]][[:punct:]]$", split.t[[1]])
#             punc2 = grep("[[:alpha:]][[:punct:]]$", split.t[[2]])
#             
#             if( length(punc1) < 1 ) punc1 = 0
#             if( length(punc2) < 1 ) punc2 = length(split.t[[2]])
#             
#             a = if( max(punc1)+1 <= length(split.t[[1]]) ) {
#               split.t[[1]][(max(punc1)+1):length(split.t[[1]])]
#             }else{
#               NULL
#             }
#             
#             b = split.t[[2]][1:min(punc2)]
#             
#             new.t = c(a, term, b)
#             new.t = new.t[which(new.t != "")]
#             new.t = paste(new.t, collapse="|:|")
#             
#             return(new.t)
#           }, s=examples))
#         }else{
#           visualData$sample = rep(" ", length(examples))
#         }
        
        visualData <- visualData[order(visualData$names), ]
        
        visualData$pointer <- rep(c(1,-1),length = nrow(visualData))
        
        visualData$line <- ifelse (visualData$pointer == 1, 
                                   (ytopbreak),
                                   (ylowbreak))
        
        visualData$counter <- as.numeric(as.factor(visualData$names))
        
        visualData$ymin.text <- ifelse(visualData$pointer == 1,
                                       ytopbreak+.005,
                                       yMin-.125)
        visualData$ymax.text <- ifelse(visualData$pointer == 1,
                                       yMax+.105,
                                       ylowbreak-.045)
        visualData$y.text <- ifelse(visualData$pointer == 1,
                                    yMax + .135,
                                    ylowbreak - .02)
        
        xMin <- (-1)
        xMax <- (max(visualData$counter) + 1)
        
        visualData$sizing <- visualData[[bubble]]
        
#         if( input$showSamples ){
#           visualData$sample <- unlist(lapply(c(1:length(visualData$sample)), 
#                                       function(n, samples, terms, heights){
#                                         sample = strsplit(visualData$sample[n], "\\|:\\|")[[1]]
#                                         term = tolower(visualData$names[n])
#                                         height = heights[n]
#                                         
#                                         fit = round(height/0.07, 0)
#                                         len = 180/length(unique(visualData$names))
#                                         
#                                         if( sum(nchar(sample), length(sample)-1) > (len*fit) ) {
#                                           pos = grep(term, sample)
#                                           punc = c(1, grep("[[:punct:]]", sample), length(sample))
#                                           
#                                           segments = matrix(nrow=length(punc), ncol=length(punc))
#                                           for( i in 1:length(punc) ){
#                                             for( j in 1:length(punc) ){
#                                               if( pos %in% punc[i]:punc[j]){
#                                                 segments[i,j] = sum(nchar(sample[punc[i]:punc[j]]), length(punc[i]:punc[j])-1)
#                                               }else{
#                                                 segments[i,j] = 0
#                                               }
#                                             }
#                                           }
#                                           
#                                           if(min(segments[which(segments > 0)]) <= len*fit){
#                                             long = which(segments == max(segments[which(segments <= len*fit)]), arr.ind=TRUE)
#                                           }else{
#                                             long = which(segments == min(segments[which(segments > 0)]), arr.ind=TRUE)
#                                           }
#                                           
#                                           p1 = min(long[,1])
#                                           p2 = min(long[which(long[,1] == p1), 2])
#                                           
#                                           sample = sample[punc[p1]:punc[p2]]
#                                           
#                                           if( sum(nchar(sample), length(sample)-1) > len*fit ){
#                                             pos = grep(term, sample)
#                                             
#                                             if( pos > length(sample)/2 ){
#                                               while( sum(nchar(sample), length(sample)-1) > len*fit){
#                                                 sample = sample[2:length(sample)]
#                                               }
#                                             }else{
#                                               while( sum(nchar(sample), length(sample)-1) > len*fit){
#                                                 sample = sample[1:(length(sample)-1)]
#                                               }
#                                             }
#                                           }
#                                         }
#                                         
#                                         lenLine = vector()
#                                         for( i in 1:length(sample) ){
#                                           lenLine[i] = sum(nchar(sample[1:i]), i-1)
#                                         }
#                                         
#                                         lines = vector()
#                                         for( i in 1:fit) {
#                                           words = sample[(lenLine < len)]
#                                           sample = sample[!(lenLine < len)]
#                                           lines[i] = paste(words, collapse=" ")
#                                           lenLine = lenLine[(lenLine > len)]-lenLine[length(words)]
#                                         }
#                                         paste(lines, collapse="\n")
#                                         
#                                       }, samples=visualData$sample, terms=visualData$names, heights=visualData$ymax.text-visualData$ymin.text))
#         }
        
        .e <- environment()
        ggplot(visualData, aes(names, sent, ymin = yMin, ymax = yMax, xmin = xMin, xmax = xMax), environment = .e) + 
          
          geom_point(aes(size = sizing), colour = 'grey10',alpha=.5,
                     stat = "identity", position = "identity") +
          
          geom_point(aes(size = sizing), colour = 'grey90',alpha=.15,
                     stat = "identity", position = "identity") +
          
          ggplot2::annotate("text", x = -0.5 , y = posneg, size=8, colour = 'grey70', label = "+", family = 'DIN Condensed Bold') +
          
          ggplot2::annotate("text", x = -0.5, y = -posneg, size=8, colour = 'grey70', label = "-", family = 'DIN Condensed Bold') +
          
          ggplot2::annotate("segment", x = visualData$counter, xend = visualData$counter, y = visualData$sent, yend = visualData$line,
                            colour = 'grey10')+
          
          ggplot2::annotate('text', x = (visualData$counter), y = (visualData$y.text), 
                            label = visualData$names, colour='white', family = 'DIN Alternate Bold', hjust = 1, size = 7) +
          
          ggplot2::annotate('text', x = visualData$counter - 1.85, y = visualData$ymax.text, 
                            label = visualData$startSample, colour = '#F54343', size = 5, family = 'DIN Alternate Bold') +
          
#           ggplot2::annotate('text', x = visualData$counter - 1.75, y = visualData$ymax.text,
#                               label = visualData$sample, colour = 'grey70', size = 6, family = 'DIN Alternate Bold', hjust = 0, vjust=1) +
          
          ggplot2::annotate('text', x = visualData$counter + .045, y = visualData$ymin.text, 
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
    }
  }, 
  height=function(){
              h=session$clientData$output_clustPlot_width*958/1278
              if(h > 958) h = 958
              return(h)
              },
  width=function(){
              w=session$clientData$output_clustPlot_width
              if(w > 1278) w = 1278
              return(w)
  })
  
  
  ##### Remove Cluser From Plot #####
  observe({
    if( !is.null(input$rmCluster) && input$rmCluster > 0 ){
      isolate({ 
        clustered = list(cls$plot_data, cls$split_data)
        clust <- rm.cluster(clustered, input$clSelect) 
      })
      cls$plot_data = clust[[1]]
      cls$split_data = clust[[2]]
    }
  })
  
  
  ##### Summarize Clusters #####
  summary <- reactive({
    if( !is.null(cls$split_data) ){
      withProgress(session, {
        setProgress(message="Summarizing Clusters")
        summ.clusters(cls$split_data, trunc = 10, myStopwords)
      })
    }
  })
  
  
  
  ##### Download Handlers #####
  # Cluster Plot #
  output$getCluster <- downloadHandler(
    filename = function() { 
      name = strsplit(input$data[1,'name'], ".json")[[1]][1]
      paste0(name, "-ClusterPlot.png")
    },
    content = function(file){
        if( !is.null(cls$plot_data) && !is.null(summary()) ){
          png(file, width=1278, height=958)
          
          visualData <- cls$plot_data
          examples = summary()
          bubble = 'total_reach'
          
          visualData$sent <- visualData$sent/10
          yMin <- ifelse (min(visualData$sent) < 0,
                          (min(visualData$sent) - .25),
                          (-.25))
          
          yMax <- ifelse (max(visualData$sent) > 0,
                          (max(visualData$sent) + .25),
                          .25)
          
          ylowbreak <- yMin + .15
          ytopbreak <- yMax - .15
          
          posneg <- ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .5, .05,
                           ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .4, .04,
                                  ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .3, .03,
                                         ifelse(max(c(ytopbreak,abs(ylowbreak))) >= .2, .02, .01))))
          
          
          visualData$startSample <- '>'
          visualData$endSample <- '//'
          
          
#           # format text samples
#           if( input$showSamples ){
#             visualData$sample = unlist(lapply(names(examples), function(s, n){ 
#               term = tolower(visualData[which(visualData['cluster'] == n ), 'names'])
#               
#               # take first example
#               t = s[[n]][1]
#               # split by term
#               split.t = strsplit(tolower(t), tolower(term))[[1]]
#               # split by whitespace
#               split.t = strsplit(split.t, "[[:space:]]")
#               # find punctuation
#               punc1 = grep("[[:alpha:]][[:punct:]]$", split.t[[1]])
#               punc2 = grep("[[:alpha:]][[:punct:]]$", split.t[[2]])
#               
#               if( length(punc1) < 1 ) punc1 = 0
#               if( length(punc2) < 1 ) punc2 = length(split.t[[2]])
#               
#               a = if( max(punc1)+1 <= length(split.t[[1]]) ) {
#                 split.t[[1]][(max(punc1)+1):length(split.t[[1]])]
#               }else{
#                 NULL
#               }
#               
#               b = split.t[[2]][1:min(punc2)]
#               
#               new.t = c(a, term, b)
#               new.t = new.t[which(new.t != "")]
#               new.t = paste(new.t, collapse="|:|")
#               
#               return(new.t)
#             }, s=examples))
#           }else{
#             visualData$sample = rep(" ", length(examples))
#           }
          
          visualData <- visualData[order(visualData$names), ]
          
          visualData$pointer <- rep(c(1,-1),length = nrow(visualData))
          
          visualData$line <- ifelse (visualData$pointer == 1, 
                                     (ytopbreak),
                                     (ylowbreak))
          
          visualData$counter <- as.numeric(as.factor(visualData$names))
          
          visualData$ymin.text <- ifelse(visualData$pointer == 1,
                                         ytopbreak+.005,
                                         yMin-.125)
          visualData$ymax.text <- ifelse(visualData$pointer == 1,
                                         yMax+.105,
                                         ylowbreak-.045)
          visualData$y.text <- ifelse(visualData$pointer == 1,
                                      yMax + .135,
                                      ylowbreak - .02)
          
          xMin <- (-1)
          xMax <- (max(visualData$counter) + 1)
          
          visualData$sizing <- visualData[[bubble]]
          
#           if( input$showSamples ){
#             visualData$sample <- unlist(lapply(c(1:length(visualData$sample)), 
#                                                function(n, samples, terms, heights){
#                                                  sample = strsplit(visualData$sample[n], "\\|:\\|")[[1]]
#                                                  term = tolower(visualData$names[n])
#                                                  height = heights[n]
#                                                  
#                                                  fit = round(height/0.07, 0)
#                                                  len = 180/length(unique(visualData$names))
#                                                  
#                                                  if( sum(nchar(sample), length(sample)-1) > (len*fit) ) {
#                                                    pos = grep(term, sample)
#                                                    punc = c(1, grep("[[:punct:]]", sample), length(sample))
#                                                    
#                                                    segments = matrix(nrow=length(punc), ncol=length(punc))
#                                                    for( i in 1:length(punc) ){
#                                                      for( j in 1:length(punc) ){
#                                                        if( pos %in% punc[i]:punc[j]){
#                                                          segments[i,j] = sum(nchar(sample[punc[i]:punc[j]]), length(punc[i]:punc[j])-1)
#                                                        }else{
#                                                          segments[i,j] = 0
#                                                        }
#                                                      }
#                                                    }
#                                                    
#                                                    if(min(segments[which(segments > 0)]) <= len*fit){
#                                                      long = which(segments == max(segments[which(segments <= len*fit)]), arr.ind=TRUE)
#                                                    }else{
#                                                      long = which(segments == min(segments[which(segments > 0)]), arr.ind=TRUE)
#                                                    }
#                                                    
#                                                    p1 = min(long[,1])
#                                                    p2 = min(long[which(long[,1] == p1), 2])
#                                                    
#                                                    sample = sample[punc[p1]:punc[p2]]
#                                                    
#                                                    if( sum(nchar(sample), length(sample)-1) > len*fit ){
#                                                      pos = grep(term, sample)
#                                                      
#                                                      if( pos > length(sample)/2 ){
#                                                        while( sum(nchar(sample), length(sample)-1) > len*fit){
#                                                          sample = sample[2:length(sample)]
#                                                        }
#                                                      }else{
#                                                        while( sum(nchar(sample), length(sample)-1) > len*fit){
#                                                          sample = sample[1:(length(sample)-1)]
#                                                        }
#                                                      }
#                                                    }
#                                                  }
#                                                  
#                                                  lenLine = vector()
#                                                  for( i in 1:length(sample) ){
#                                                    lenLine[i] = sum(nchar(sample[1:i]), i-1)
#                                                  }
#                                                  
#                                                  lines = vector()
#                                                  for( i in 1:fit) {
#                                                    words = sample[(lenLine < len)]
#                                                    sample = sample[!(lenLine < len)]
#                                                    lines[i] = paste(words, collapse=" ")
#                                                    lenLine = lenLine[(lenLine > len)]-lenLine[length(words)]
#                                                  }
#                                                  paste(lines, collapse="\n")
#                                                  
#                                                }, samples=visualData$sample, terms=visualData$names, heights=visualData$ymax.text-visualData$ymin.text))
#           }
          
          .e <- environment()
          print(ggplot(visualData, aes(names, sent, ymin = yMin, ymax = yMax, xmin = xMin, xmax = xMax), environment = .e) + 
            
            geom_point(aes(size = sizing), colour = 'grey10',alpha=.5,
                       stat = "identity", position = "identity") +
            
            geom_point(aes(size = sizing), colour = 'grey90',alpha=.15,
                       stat = "identity", position = "identity") +
            
            ggplot2::annotate("text", x = -0.5 , y = posneg, size=8, colour = 'grey70', label = "+", family = 'DIN Condensed Bold') +
            
            ggplot2::annotate("text", x = -0.5, y = -posneg, size=8, colour = 'grey70', label = "-", family = 'DIN Condensed Bold') +
            
            ggplot2::annotate("segment", x = visualData$counter, xend = visualData$counter, y = visualData$sent, yend = visualData$line,
                              colour = 'grey10')+
            
            ggplot2::annotate('text', x = (visualData$counter), y = (visualData$y.text), 
                              label = visualData$names, colour='white', family = 'DIN Alternate Bold', hjust = 1, size = 7) +
            
            ggplot2::annotate('text', x = visualData$counter - 1.85, y = visualData$ymax.text, 
                              label = visualData$startSample, colour = '#F54343', size = 5, family = 'DIN Alternate Bold') +
            
#             ggplot2::annotate('text', x = visualData$counter - 1.75, y = visualData$ymax.text,
#                               label = visualData$sample, colour = 'grey70', size = 6, family = 'DIN Alternate Bold', hjust = 0, vjust=1) +
                
            ggplot2::annotate('text', x = visualData$counter + .045, y = visualData$ymin.text, 
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
            scale_color_identity() )
        dev.off()
      }
    },
    contentType = 'image/png'
  )
  
  
  # Cluster Summary #
  output$summCluster <- downloadHandler(
    filename = function() { 
      name = strsplit(input$data[1,'name'], ".json")[[1]][1]
      paste0(name, "-Summary.txt")
    },
    content = function(file){
      mapply(function(list, name){
        name = paste0(name, "-", cls$plot_data[which(cls$plot_data['cluster'] == name), 'names'])
        write( paste0(name, "\n", paste(unlist(list), collapse="\n"), "\n"), file, append=TRUE, ncolumns=1000)
      }, summary(), names(summary()), SIMPLIFY=F, USE.NAMES=F)
    })
  
  
  
  
  
  
  ############### 4. Explore Features ###############
  # Read data file
  termScrape <- reactive({
    # Read in the data file
    read.csv(input$termScrape[1, 'datapath'])
  })

  # Plot Termscrape
  output$termScrapePlot <- renderPlot({
    if( !is.null(input$termScrape) && !is.null(input$direction) && !is.null(input$tsSparse) && !is.null(input$tsN) ){
      # Parse Direction
      direction = switch(input$direction,
                         "Positive" = "pos",
                         "Negative" = "neg")
     # Make Plot
     extract.features(termScrape(), direction, input$tsSparse, input$tsN)  
    }
  })
  
  
  
  
  ############### Observers ###############
  
  ##### Sync Sparse Slider with Plus/Minus #####
  observe({
    if( input$sparseMinus > 0 ) {
      updateSliderInput(session, 'sparse', value=isolate(input$sparse)-0.01)
    }
  })
  observe({
    if( input$sparsePlus > 0 ) {
      updateSliderInput(session, 'sparse', value=isolate(input$sparse)+0.01)
    }
  })
  
  ##### Sync n Slider with Plus/Minus #####
  observe({
    if( input$nMinus > 0 ) {
      updateSliderInput(session, 'n', value=isolate(input$n)-1)
    }
  })
  observe({
    if( input$nPlus > 0 ) {
      updateSliderInput(session, 'n', value=isolate(input$n)+1)
    }
  })
  
  ##### Sync k Slider with Plus/Minus #####
  observe({
    if( input$kMinus > 0 ) {
      val = if(isolate(input$k)-1 < 0) 0 else isolate(input$k)-1
      updateSliderInput(session, 'k', value=val)
    }
  })
  observe({
    if( input$kPlus > 0 ) {
      val = if(isolate(input$k)+1 > 10) 10 else isolate(input$k)+1
      updateSliderInput(session, 'k', value=val)
    }
  })

  ##### Sync Term Scrape Sparse Slider with Plus/Minus #####
  observe({
    if( input$tsSparseMinus > 0 ) {
      updateSliderInput(session, 'tsSparse', value=isolate(input$tsSparse)-0.01)
    }
  })
  observe({
    if( input$tsSparsePlus > 0 ) {
      updateSliderInput(session, 'tsSparse', value=isolate(input$tsSparse)+0.01)
    }
  })

  ##### Sync Term Scrape N Slider with Plus/Minus #####
  observe({
    if( input$tsNMinus > 0 ) {
      updateSliderInput(session, 'tsN', value=isolate(input$tsN)-1)
    }
  })
  observe({
    if( input$tsNPlus > 0 ) {
      updateSliderInput(session, 'tsN', value=isolate(input$tsN)+1)
    }
  })
  
})
