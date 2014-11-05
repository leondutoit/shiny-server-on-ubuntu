# options(scipen = 999)
# 
# 
# library(igraph)
# library(ggplot2)
# library(reshape2)
# library(wordcloud)
# library(tm)
# library(Rweka)


heat.map <- function(matrix){
  # Args:
  #   matrix: the weighted doc-term matrix most commonly returned from process.text()
  # Returns:
  #   returns a ggplot object that shows the weight of terms relative to the documents
  # Additional Work:
  #   sort docs based on similarity using extracat pkg or simm matrix
  #   add title
  df <- as.data.frame(t(matrix))
  df$term <- rownames(df)
  colnames(df) <- as.character(c(1:(ncol(df)-1),"term"))
  df.m <- melt(df)
  p <- ggplot(df.m, aes(variable, term)) + 
    geom_tile(aes(fill = value)) +
    scale_fill_gradient(low = "#FAFAD2", high = "#F54343") +
    theme(panel.background = element_rect(fill = "#314050",colour = NA),
          plot.background = element_rect(fill = "#314050",colour = NA),
          text = element_text(family = 'DIN Condensed Bold',size=20),
          axis.title = element_blank(),
          axis.text.y = element_text(size = 18, vjust = .5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  return(p)
}




term.network <- function(matrix){
  # Args:
  #   matrix: the adjacency matrix returned using the process.text() with n = 1
  # Returns:
  #   returns a picture of how terms interact within the conversation
  # Additional Work:
  #   Allow users to costomize how many terms they would like to see
  #   Add title
  g <- graph.adjacency(matrix, weighted = T, mode = "undirected")
  
  # remove loops
  g <- simplify(g)
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  
  # set seed to make the layout reproducible
  set.seed(3952)
  layout1 <- layout.fruchterman.reingold(g)
  
  V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
  V(g)$label.color <- "white"
  V(g)$frame.color <- NA
  egam <- (log(E(g)$weight)+.2) / max(log(E(g)$weight)+.2)
  E(g)$color <- '#F54343'
  E(g)$width <- egam
  # plot the graph in layout1
  par(bg = "#314050")
  plot(g, layout = layout1,vertex.color=rgb(.1,.1,.1,.5))
  
}



word.cloud <- function(matrix){
  # Args:
  #   matrix: transpose of the document term matrix from process.text()
  # Returns:
  #   returns a wordcloud of non-sparse terms from the corpus
  matrix <- t(matrix)
  wordFreq <- sort(rowSums(matrix), decreasing=TRUE)
  # word cloud
  set.seed(375) # to make it reproducible
  grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
  par(bg = "#314050") # set background color
  wordcloud(words = names(wordFreq), freq = wordFreq, min.freq = 3, random.order = F,
            colors = '#F54343')
}



# 
# search.corpus <- function(matrix, term, distance){
#   # Args:
#   #   corpus: the character of vector strings you want to scan
#   #   term: the term interested in searching for
#   #   distance: the lower threshold to limit the query against
#   # Return:
#   #   return a plot of the terms as a bar chart based on how often the occur
#   # Additional Work:
#   #   Add Title
#   #   Allow more customization to search
#   associations <- as.data.frame(findAssocs(matrix, term, distance))
#   associations$term <- rownames(associations)
#   names(associations) <- c("score", "term")
#   associations <- associations[1:25,]
#   associations$term <- factor(associations$term, levels = unique(as.character(associations$term)), ordered = TRUE )
#   
#   p <- ggplot(data = associations, aes(x = term, y = score)) +
#     geom_bar(stat = "identity", fill = "#F54343", alpha = .65) +
#     coord_flip() +
#     theme(panel.background = element_rect(fill = "#314050",colour = NA),
#           plot.background = element_rect(fill = "#314050",colour = NA),
#           axis.text.y = element_text(family = 'DIN Condensed Bold', size=20,colour = 'white'),
#           axis.text.x = element_blank(),
#           axis.ticks = element_blank(),
#           axis.title = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position = "none")
#   
#   return(p)
# }



pull.geo <- function(json){
  # enter user JSON with Geo Tags to plot on US map
  # args: 
  #   json: is the read in json file
  # returns:
  #   plot with US map with neg/pos sentiment sized by klout
  # additional work:
  #   expand plots beyond US & provide plot size flexibility
  geo <- ldply(lapply(json[], function(y) unlist(list("post_id" = y$interaction$id,
                                                      "user_id" = y$interaction$author$id,
                                                      "content" = y$interaction$content,
                                                      "sentiment" = y$salience$content$sentiment,
                                                      "klout" = y$klout$score,
                                                      "lon" = y$twitter$geo$longitude,
                                                      "lat" = y$twitter$geo$latitude))),rbind)
  geo <- geo %>%
    dplyr::mutate(klout = as.numeric(as.character(klout)),
                  lon = as.numeric(as.character(lon)),
                  sentiment = as.numeric(as.character(sentiment)),
                  lat = as.numeric(as.character(lat)))
  
  geo_pos <- geo[which(geo$sent > 0),]
  geo_neg <- geo[which(geo$sent < 0),]
  
  all_states <- map_data("state")
  #plot all states with ggplot
  ggplot() + 
    geom_polygon(data = all_states, aes(x = long, y = lat, group = group), colour = "grey10", fill = "grey50") +
    geom_point(data = geo_neg, aes(x = lon, y = lat, size = klout), color = "#F54343", alpha = .5) + 
    geom_point(data = geo_pos, aes(x = lon, y = lat, size = klout), color = "#73BC4F", alpha = .5) + 
    scale_size(range = c(1, 5)) +
    theme(panel.background = element_rect(fill = "#314050",colour = NA),
          plot.background = element_rect(fill = "#314050",colour = NA),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          legend.position="none",
          panel.grid  = element_blank()) 
}




plot.termscrape <- function(filename, myStopwords, add_list){
  # function to read CSV output from python script and create plot
  # Args:
  #     filename: the filename of the csv file containing the necessary data
  # Returns: 
  #     a horizontal bar plot ranked by frequency of terms occuring in the data
  # Runs assuming csv is in same folder as other project data
  posterior <- read.csv(filename, header=FALSE)
  posterior$V1 <- tolower(posterior$V1)
  posterior <- posterior[order(-posterior$V2), ]
  rmMisc <- which(posterior$V1 %in% myStopwords)
  posterior <- posterior[-rmMisc, ]
  
  posterior <- posterior %>%
    group_by(V1) %>%
    summarise(V2 = sum(V2)) %>%
    arrange(desc(V2))
  
  posterior <- posterior[1:25,]
  posterior$V1 <- factor(posterior$V1, levels = unique(as.character(posterior$V1)), ordered = TRUE )
  
  
  bar_plot = ggplot(posterior,aes(V1,V2))+
    geom_bar(stat = "identity", fill = "#F54343", alpha = .65)+
    coord_flip() +
    theme(panel.background = element_rect(fill = "#314050",colour = NA),
          plot.background = element_rect(fill = "#314050",colour = NA),
          axis.text.y=element_text(family='DIN Condensed Bold',size=20,colour='white'),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
  return(bar_plot)
}



extract.features <- function(data, direction, sparse, n){
  # Args:
  #   data: amazon data read in from CSV file
  #   sparse: the minimal allowable sparsity of the data
  #   direction: 'pos' or 'neg' based on feature extraction needed for pos association/neg
  #   n: the amount of features to return
  # Return:
  #   return bar plot with features and ranked by the features odds ratios of the corresponding direction
  # Additional Work:
  #   Use data to train model, and test in Analysis function
  #   Improve functionality to account for very postive vs very negative
  
  #data <- read.csv(filename)
  data$sent <- ifelse (data$rating > .5, "pos",
                       ifelse (data$rating < .5, "neg", "neutral"))
  
  corpus <- Corpus(VectorSource(as.vector(data[["text"]])))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, tm::stopwords(kind='en'))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument)
  
  options(mc.cores = 1)
  TrigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))}
  tdf <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer, 
                                                   weighting = function(mat)weightTfIdf(mat, normalize=FALSE)))
  
  tdf <- removeSparseTerms(tdf, sparse = sparse)
  
  tdf <- as.data.frame(as.matrix(t(tdf)))
  options(warn = -1)
  tdf <- cbind(tdf, data$sent)
  
  
  split_tdf <- split(tdf, as.factor(data$sent))
  
  agg <- lapply(split_tdf, function(x)colSums(x[,1:(ncol(tdf)-1)]))
  
  negative <- as.vector(agg[1])
  
  positive <- as.vector(agg[2])
  
  
  df <- data.frame(negative, positive)
  df$tot <- rowSums(df)
  df$pw <- df$tot / sum(df$tot)
  df$p_neg <- sum(df$neg) / sum(df$tot)
  df$p_pos <- sum(df$pos) / sum(df$tot)
  
  
  df$p_w_neg <- df$neg / sum(df$neg)
  df$p_w_pos <- df$pos / sum(df$pos)
  
  
  df$p_neg_w <- (df$p_w_neg * df$p_neg) / df$pw
  df$p_pos_w <- (df$p_w_pos * df$p_pos) / df$pw
  
  
  df$odds_neg <- df$p_neg_w / (1 - df$p_neg_w)
  df$odds_pos <- df$p_pos_w / (1 - df$p_pos_w)
  
  df$factor <- rownames(df)
  
  plot_data <- df
  if (direction == 'pos'){
    col = "#73BC4F"
    plot_data$dir = plot_data$p_pos_w
  } else {
    col = "#F54343"
    plot_data$dir = plot_data$p_neg_w
  }
  
  plot_data <- plot_data[order(-plot_data$dir),][1:n,]
  plot_data$factor <- factor(plot_data$factor, levels = unique(as.character(plot_data$factor)), ordered = TRUE)
  rownames(plot_data) <- c(1:n)
  
  
  feature = ggplot(plot_data, aes(factor, dir)) +
    geom_bar(stat = "identity", fill = col, alpha = .65)+
    coord_flip() +
    theme(panel.background = element_rect(fill = "#314050",colour = NA),
          plot.background = element_rect(fill = "#314050",colour = NA),
          axis.text.y=element_text(family='DIN Condensed Bold',size=20,colour='white'),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
  
  return(feature)
}