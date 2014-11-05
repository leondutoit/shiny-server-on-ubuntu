# options(scipen = 999)
# 
# library(stringr)
# library(jsonlite)
# library(RWeka)
# library(tm)
# library(plyr)
# library(dplyr)
# library(SnowballC)


# read in Stopwords
stopwords <- function(wd){
  # Function to read in list of stopwords in the folder with .Rproj and .R files
  # Args:
  #   wd: the working directory where the RProject is stored
  # Returns:
  #   stop_words: a character vector of stopwords created exclusively for use when analyzing social data
  setwd(wd)
  stop_words <- as.vector(scan(file = "stop_words_social.txt", what = "character"))
  return(stop_words)
}


#read in the JSON
parse.json <- function(file, section = NULL, shiny=F){
  # Reads in raw json file from a single data source (i.e. twitter, facebook)
  # Args:
  #   JSON_file : name of the file and location of JSON (i.e. should be the same as wd of project)
  #   section : specify a single section of JSON to work with (i.e. 'interaction', 'twitter', 'links')
  # Returns:
  #   json : the parsed json with a list for each json object
  #   selection : list of selected section for each json object. (if NULL then does not exist)
  # Additional work:
  #   add a layer of cleaning that deals with html tags for blogs/boards/wordpress
  c <- file(file, "r") #establish connection
  l <- readLines(c, -1L)
  json <- mapply(function(x,n){
    if(shiny) setProgress(value=n)
    fromJSON(x)
  }, l, c(1:length(l)/length(l)), SIMPLIFY=F, USE.NAMES=F) # read json into list

  selection<-list()
  if (length(section) > 0) {
    for (j in 1:length(json)){
      selection[[j]] <- json[[j]][section]
    }
    paste('found section', names(selection[[1]]), sep = " ")
  }
  close(c)
  
  setProgress(value=1)
  
  if(length(selection > 0)) {
    return(list(json,selection))
  } else {
    return(json)
  }
}


clean.json <- function(json, shiny=F){
  if(shiny) setProgress(detail="Cleaning up text", value=0.01)
  for (i in 1:length(json)) {
    json[[i]]$interaction$content <- iconv(as.character(json[[i]]$interaction$content), to="UTF-8") # convert encoding 
    json[[i]]$interaction$content <- str_replace_all(string = json[[i]]$interaction$content , pattern = "\"*" , repl = "") 
    json[[i]]$interaction$content <- str_replace_all(string = json[[i]]$interaction$content , pattern = "http[[:graph:]]*" , repl = "") ##remove URL
    json[[i]]$interaction$content <- str_replace_all(string = json[[i]]$interaction$content , pattern = "[[:cntrl:]]*" , repl = "") #remove unicode
    json[[i]]$interaction$content <- str_replace_all(string = json[[i]]$interaction$content , pattern = ".*@.*: " , repl = "") ## remove mentions in text
    json[[i]]$interaction$content <- str_replace_all(string = json[[i]]$interaction$content , pattern = "@.*[[:blank:]]+?" , repl = "") #remove usernames in text
    json[[i]]$interaction$content <- str_replace_all(string = json[[i]]$interaction$content , pattern = "\n+?" , repl = "") #remove new line symbol
    json[[i]]$interaction$content <- str_replace_all(string = json[[i]]$interaction$content , pattern = "amp;+?" , repl = "") #remove "amp;" from "amp;&"
    json[[i]]$interaction$content <- str_replace_all(string = json[[i]]$interaction$content , pattern = "^\\s+|\\s+$" , repl = "") #remove whitespace
    
    if(shiny) setProgress(value=i/length(json))
  }
}


#filter out unwanted content
filter.rm <- function(json, filter){
  # Filter out interactions based on terms supplied by user
  # Args:
  #   json : list of json file with all the interactions to filter against
  #   filters : character string to remove
  # Returns:
  #   json : list of line delimited json interactions with filtered out terms

  content <- unlist(lapply(json[], FUN = function(x) unlist(as.character(x$interaction$content)))) #character vector with all content
  content <- iconv(content, to="UTF-8") #UTF-8 encoding
  test <- grep(filter, content, ignore.case = TRUE) # identify strings to remove
  if (length(test) > 0){
    json <- json[-test] ## remove obs
  } else { 
    warning(" No string matching '",filter,"' was found ")
  }
  return(json)
}



## split data up based on existence of content
filter.add<- function(json, filter){
  # user json file to split JSON files based on the existence of a term
  # args: 
  #   json: is the read in json file
  #   filters : character string to pull
  # returns:
  #   term_json:nested list of JSON, which each list representing the all observations associated with that term
  content <- unlist(lapply(json[], FUN = function(x) unlist(as.character(x$interaction$content))))
  content <- iconv(as.character(content), to="UTF-8")
  
  test <- grep(filter, content, ignore.case = TRUE)
  if (length(test) > 0){
    add <- json[test]
    return(add)
  } else { 
    warning(" No string matching '",filter,"' was found ")
    return(json)
  }
}



split.tags <- function(json){
  # user json file to split JSON files based on the tags created using Datasift or manually
  # args: 
  #   json: is the read in json file with the tags
  # returns:
  #   nested list of JSON, which each list representing the all observations associated with that tag
  # Additional Work
  #   replace loop with alternative method
  #   automatically create R object for each new list named by the tag
  test <- lapply(json, function(x)x$interaction$tags) ## pull tags 
  tags <- levels(as.factor(unlist(test)))
  
  split <- list()
  for (i in 1:length(tags)) {
    tag <- tags[i]
    #print(sprintf("Tag %i: %s", i, tag))
    boolean <- lapply(json, function(x) tag %in% x$interaction$tags) ## page through list looking for the user defined tag in the observation
    split[[i]] <- json[which(boolean == TRUE)] ## set obs into list object i
    
  }
  names(split) <- tags
  
  return(split)
}



split.type <- function(json){
  # user json file to split JSON files based on the type of data from Datasift
  # args: 
  #   json: is the JSON file read in using the parse.json file
  # returns:
  #   nested list of JSON, which each list representing the all observations associated with that data source
  # Additional Work
  #   replace loop with alternative method
  #   automatically create R object for each new list named by the data source
  test <- lapply(json, function(x)x$interaction$type)
  types <- levels(as.factor(unlist(test)))
  
  split <- list()
  for (i in 1:length(types)) {
    type <- types[i]
    #print(sprintf("Source %i: %s", i, type))
    boolean <- lapply(json, function(x) type %in% x$interaction$type) ## page through list looking for data source in the observation
    split[[i]] <- json[which(boolean == TRUE)]  ## set obs into list object i
    names(split[[i]]) <- type
    
  }
  names(split) <- types

  return(split)
}



# Agg based on twitter data=
agg.twitter <- function(json, split = T){ 
  # aggregate the data based on Twitter RT
  # Args:
  #   json : json file with data from query
  # Returns:
  #   returns the data with the reach and rt counts for all unique content
  # Additional Work:
  #   data.table to speed up aggregation of data
  twitter <- ldply(lapply(json[], function(y) unlist(list("content" = y$interaction$content,
                                                          "type" = y$interaction$type,
                                                          "rted_post_id" = y$twitter$retweeted$id,
                                                          "post_id" = y$twitter$id,
                                                          "user_id" = y$twitter$user$id,
                                                          "rt_user_id" = y$twitter$retweet$user$id,
                                                          "rted_user_id" = y$twitter$retweeted$user$id,
                                                          "rt_followers_count" = y$twitter$retweet$user$followers_count,
                                                          "rted_followers_count" = y$twitter$retweeted$user$followers_count,
                                                          "followers_count" = y$twitter$user$followers_count,
                                                          "rt_count" = y$twitter$retweet$count,
                                                          "sentiment" = y$salience$content$sentiment,
                                                          "klout" = y$klout$score))),rbind)
  if (split == T){
    twitter <- dplyr::select(twitter, -.id)
    original <- twitter[which(is.na(twitter$user_id)==F),]  
    interaction <- original %>%
      dplyr::select(post_id, content, sentiment, klout, followers_count) %>%
      dplyr::mutate(sent = as.numeric(as.character(sentiment)),
                    influence = as.numeric(as.character(klout)),
                    reach = as.numeric(as.character(followers_count)),
                    engage = 1) %>%
      dplyr::select(post_id, content, sent, influence, reach, engage)
    
    if (length(which(is.na(twitter$user_id)==T)) > 0) {
      rt <- twitter[which(is.na(twitter$user_id)==T),]
      rt <- rt %>%
        dplyr::select(rted_post_id, content, rt_followers_count,
                      rt_count, sentiment, klout) %>%
        dplyr::group_by(rted_post_id, content) %>%
        dplyr::summarise(engage = mean(as.numeric(as.character(rt_count)), na.rm = TRUE),
                         reach = sum(as.numeric(as.character(rt_followers_count)), na.rm = TRUE),
                         sent = mean(as.numeric(as.character(sentiment)), na.rm = TRUE),
                         influence = mean(as.numeric(as.character(klout)), na.rm = TRUE)) %>%
        dplyr::mutate(post_id = rted_post_id) %>%
        dplyr::select(post_id, content, sent, influence, reach, engage)
      twitter <- join(rt, interaction, type = 'full', by = "post_id")
    } else {
      twitter <- interaction
    }
  } else {
    original <- twitter[which(is.na(twitter$user_id)==F),]  
    interaction <- original %>%
      dplyr::select(post_id, content, sentiment, klout, followers_count) %>%
      dplyr::mutate(sent = as.numeric(as.character(sentiment)),
                    influence = as.numeric(as.character(klout)),
                    reach = as.numeric(as.character(followers_count)),
                    engage = 1) %>%
      dplyr::select(post_id, content, sent, influence, reach, engage)
    
    if (length(which(is.na(twitter$user_id)==T)) > 0) {
      rt <- twitter[which(is.na(twitter$user_id)==T),]
      rt <- rt %>%
        dplyr::select(rted_post_id, content, rt_followers_count,
                      rt_count, sentiment, klout) %>%
        dplyr::group_by(rted_post_id, content) %>%
        dplyr::summarise(engage = mean(as.numeric(as.character(rt_count)), na.rm = TRUE),
                         reach = sum(as.numeric(as.character(rt_followers_count)), na.rm = TRUE),
                         sent = mean(as.numeric(as.character(sentiment)), na.rm = TRUE),
                         influence = mean(as.numeric(as.character(klout)), na.rm = TRUE)) %>%
        dplyr::mutate(post_id = rted_post_id) %>%
        dplyr::select(post_id, content, sent, influence, reach, engage)
      twitter <- join(rt, interaction, type = 'full', by = "post_id")
    } else {
      twitter <- interaction
    }
  }
  
  t <- twitter %>%
    dplyr::group_by(content) %>%
    dplyr::summarise(engage = mean(engage, na.rm = TRUE),
                     reach = sum(reach, na.rm = TRUE),
                     sent = mean(sent, na.rm = TRUE),
                     influence = mean(influence, na.rm = TRUE))
  return(t)  
}




#for aggregating facebook data
agg.facebook <- function(json, split = T){ 
  # aggregate the data based on matching content
  # Args:
  #   json : json file with data from query
  # Returns:
  #   returns data.frame with facebook and agg information to work with
  # Additional Work:  
  #   merge content using a similarity matrix w/ a user defined threshold of similarity
  facebook <- ldply(lapply(json[], function(y) unlist(list("id" = y$facebook$id,
                                                           "type" = y$facebook$type,
                                                           "auth_id" = y$facebook$author$hash_id,
                                                           "auth_type" = y$facebook$author$type,
                                                           "content" = y$facebook$message,
                                                           "sentiment" = y$salience$content$sentiment,
                                                           "klout" = y$klout$score))),rbind)
  if (split == T){
    facebook <- dplyr::select(facebook, -.id)
    interaction <- facebook %>%
      dplyr::group_by(content) %>%
      dplyr::summarise(engage = n(),
                       sent = mean(sentiment, na.rm = TRUE))                                                                                                                 
  } else {
    interaction <- facebook %>%
      dplyr::group_by(content) %>%
      dplyr::summarise(engage = n(),
                       sent = mean(sentiment, na.rm = TRUE))    
  }
  return(interaction)
}
  



# for aggregating tumblr data
agg.tumblr <- function(json){
  # aggregate the data based on matching content
  # Args:
  #   json : json file with data from query
  # Returns:
  #   returns data.frame with tumblr and agg information to work with
  # Additional Work:  
  #   merge content using a similarity matrix w/ a user defined threshold of similarity
  tumblr <- ldply(lapply(json[], function(y) unlist(list("content" = y$interaction$content,
                                                         "type" = y$tumblr$type,
                                                         "blogid" = y$tumblr$blogid,
                                                         "root_postid" = y$tumblr$root$postid,
                                                         "post_url" = y$tumblr$post_url,
                                                         "likes" = y$tumblr$meta$like_global,
                                                         "sentiment" = y$salience$content$sentiment,
                                                         "klout" = y$klout$score))),rbind)
  if (split == T){
    tumblr <- dplyr::select(tumblr, -.id)
    interaction <- tumblr %>%
      dplyr::group_by(root_postid) %>%
      dplyr::summarise(engage = n(),
                       likes = max(likes, na.rm = T),
                       sent = mean(sentiment, na.rm = T),
                       influence = mean(klout, na.rm = T))
  } else {
    interaction <- tumblr %>%
      dplyr::group_by(root_postid,content) %>%
      dplyr::summarise(engage = n(),
                       likes = max(likes, na.rm = T),
                       sent = mean(sentiment, na.rm = T),
                       influence = mean(klout, na.rm = T))
  }
  
  return(interaction)                                                       
}



#for aggregating wordpress data
agg.wordpress <- function(json){
  # aggregate the data based on matching content
  # Args:
  #   json : json file with data from query
  # Returns:
  #   returns data.frame with wordpress and agg information to work with
  # Additional Work:  
  #   merge content using a similarity matrix w/ a user defined threshold of similarity
  wordpress <- ldply(lapply(json[],function(x) unlist(list("author_id" = x$wordpress$author$id, 
                                                           "blog_id" = x$wordpress$article$blog_id,
                                                           "blog_url" = x$wordpress$link,
                                                           "blog_summ" = x$wordpress$blog$summary,
                                                           "comment_count" = x$wordpress$article$comment_count,
                                                           "content" = x$wordpress$content,
                                                           "sentiment" = y$salience$content$sentiment,
                                                           "klout" = y$klout$score))),rbind)
  if (split == T){
    wordpress <- dplyr::select(wordpress, -.id)
    interaction <- wordpress %>%
      dplyr::group_by(content) %>%
      dplyr::summarise(engage = n(),
                       comment = max(comment_count, na.rm = T),
                       sent = mean(sentiment, na.rm = T),
                       influence = mean(klout, na.rm = T))
  } else {
    interaction <- wordpress %>%
      dplyr::group_by(content) %>%
      dplyr::summarise(engage = n(),
                       comment = max(comment_count, na.rm = T),
                       sent = mean(sentiment, na.rm = T),
                       influence = mean(klout, na.rm = T))
  }
  return(interaction)
}


#aggregate interaction content
agg.content <- function(json){
  # aggregate the data based on matching content
  # Args:
  #   json : json file with data from query
  # Returns:
  #   returns json objects with content and agg information to work with
  # Additional Work:  
  #   merge content using a similarity matrix w/ a user defined threshold of similarity
  interaction <- ldply(lapply(json[],function(x) unlist(list("type" = x$interaction$type, 
                                                              "id" = x$interaction$author$id, 
                                                              "user" = x$interaction$author$username, 
                                                              "content" = x$interaction$content, 
                                                              "sentiment" = x$salience$content$sentiment, 
                                                              "klout" = x$klout$score,
                                                              "count" = 1))),rbind)
  interaction <- interaction %>%
    dplyr::mutate(klout = as.numeric(as.character(klout)),
           sentiment = as.numeric(as.character(sentiment)),
           count = as.numeric(as.character(count))) %>%
    dplyr::group_by(content) %>%
    dplyr::summarise(engage = sum(count, na.rm = T),
                     sent = mean(sentiment, na.rm = T),
                     klout = mean(klout, na.rm = T))
  
  return(interaction)
}




#agg the links shared through all of the interactions
rank.links <- function(json){
  # aggregate the data based on matching links
  # Args:
  #   json : json file with data from query
  # Returns:
  #   returns data aggregated by the links associated with the interaction
  #   * this function does not return content that does not contain a link use inter
  # Additional Work:
  #   scrape the links content to help build better picture
  link_data <- ldply(lapply(json[],function(x) unlist(list("type" = x$interaction$type, 
                                                       "id" = x$interaction$id,
                                                       "content" = x$interaction$content,
                                                       "sentiment" = x$salience$content$sentiment, 
                                                       "klout" = x$klout$score,
                                                       "count" = 1,
                                                       "link" = x$links$normalized_url[[1]],
                                                       "title" = x$links$title[[1]],
                                                       "domain" = sub(".*?://(.*?)/.*", "\\1", 
                                                                      x$links$normalized_url[[1]])))),rbind)
  
  links <- link_data[which(is.na(link_data$link) == F), ]
  
  urls <- links %>%
    dplyr::group_by(domain,link,title) %>%
    dplyr::summarise(influence = mean(as.numeric(as.character(klout)), na.rm = T),
                     feeling = mean(as.numeric(as.character(sentiment)), na.rm = T),
                     frequency = sum(as.numeric(as.character(count)), na.rm = T)) %>%
    dplyr::arrange(desc(frequency))
    
  return(urls)
}



rank.users <- function(json){
  # aggregate the data based on matching users
  # Args:
  #   json : json file with data from query
  # Returns:
  #   returns data aggregated by the users associated with the interaction
  users <- ldply(lapply(json[], function(x) unlist(list("content" = x$interaction$content,
                                                        "type" = x$interaction$type,
                                                        "auth_type" = x$interaction$author$type,
                                                        "auth_name" = x$interaction$author$name,
                                                        "auth_hash_id" = x$interaction$author$hash_id,
                                                        "auth_id" = x$interaction$author$id,
                                                        "sentiment" = x$salience$content$sentiment,
                                                        "klout" = x$klout$score,
                                                        "count" = 1))),rbind)
  u <- users %>% 
    dplyr::group_by(auth_id, auth_hash_id, auth_name) %>%
    dplyr::summarise(influence = mean(as.numeric(as.character(klout)), na.rm = T),
              feeling = mean(as.numeric(as.character(sentiment)), na.rm = T),
              involvement = sum(as.numeric(as.character(count)), na.rm = T)) %>%
    dplyr::arrange(desc(influence)) 
}





#Exploring user participation across conversations
user.topic.matrix <- function(data){
  # using user data split by topics/followers/etc to build matrix of user by type of participation
  # Arg:
  #   x : a split data set for the users from each of the participation topics
  # Returns: 
  #   m : simple_triplet_matrix of users by participation topic
  # Additional Work
  #   user selection for count metric to compare against
  users <- mclapply(json[], function(x)ldply(lapply(x[], function(y) unlist(list("content" = y$interaction$content,
                                                                               "type" = y$interaction$type,
                                                                               "screen_name" = y$interaction$author$username,
                                                                               "avatar" = y$interaction$author$avatar,
                                                                               "auth_name" = y$interaction$author$name,
                                                                               "gender" = y$demographic$gender,
                                                                               "interaction_id" = y$interaction$id,
                                                                               "user_id" = y$interaction$author$id,
                                                                               "sentiment" = y$salience$content$sentiment,
                                                                               "klout" = y$klout$score))),rbind))
  

  all <- do.call("rbind", users)
  all$id <- rep(names(users), sapply(users, nrow))
  
  agg <- all %>% 
    dplyr::group_by(id,user_id) %>%
    dplyr::summarise(count = n(),
      influence = max(klout, na.rm = T),
      sent = mean(sentiment, na.rm = T)) %>%
    dplyr::select(id,user_id,count)

  
  agg <- agg[-which(is.na(agg$user_id) == T), ]
  
  t <- split(agg, agg$id)
  count_list <- mclapply(unname(t),
                         function(x) setNames(x$count, 
                                              as.character(x$user_id)))
  
  v <- unlist(count_list)
  i <- names(v)
  all_id <- sort(unique(i))
  i <- match(i, all_id)
  j <- rep(seq_along(t), sapply(count_list, length))
  
  m <- simple_triplet_matrix(i, j, v = as.numeric(v),
                             nrow = length(all_id),
                             ncol = length(t),
                             dimnames =
                               list(Users = all_id,
                                    Topics = unlist(col)))
  m2 <- t(m)
  m2[m2 >= 1] <- 1
  adj_mat <- m2 %*% t(m2)
  return(list("user_tag" = m, "tag_user" = m2, "adj_mat" = adj_mat))
}



process.text <- function(d, sparse = .99, sample = 10000, n = 3, myStopwords = stopwords, add_list = NULL) {
  # function to pre-process text for analysis on exploration
  # Args: 
  #   d: select aggregated data to user for content analysis
  #   text: the variable/column of text to process
  #   sparse: set level of sparsity to reduce tdm down too.
  #   sample: how many variables to sample
  #   addList: list of terms to add to stopwords list
  # Returns:
  #   corpus: corpus of all text docs fully reduced
  #   raw_mat: the tdm with all the 1:3-gram tokens and there weights
  #   tdm: tdm with sparsity removed and ready for analysis
  #   adj_mat: the term adjacency matrix
  myStopwords <- c(myStopwords,add_list)
  
  if (nrow(d) > sample) {training<-d[sample(1:nrow(d), sample, replace=FALSE),]}
  if (nrow(d) < sample) {training<-d}
  
  training[["content"]] <- iconv(as.character(training[["content"]]), to="UTF-8") # convert encoding 
  corpus <- Corpus(VectorSource(as.vector(training[["content"]])))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  corpus<-tm_map(corpus,removeURL)
  corpus <- tm_map(corpus, removeWords,myStopwords)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, PlainTextDocument)

  options(mc.cores = 1)
  TrigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = n))}
  mat <- TermDocumentMatrix(corpus,control = list(tokenize = TrigramTokenizer, 
                                        weighting = function(mat)weightTfIdf(mat, normalize=FALSE)))
  
  mat2 <- removeSparseTerms(mat, sparse = sparse)
  m2 <- as.matrix(mat2)
  to_rm <- which(colSums(m2) == 0)
  m2 <- m2[,-to_rm]
  d <- training[-to_rm,]
  m3 <- t(m2)
  dims <- ncol(m3)
  print(dims)
  m2[m2 >= 1] <- 1
  # transform into a term-term adjacency matrix
  adj_mat <- m2 %*% t(m2)
  if( !is.null(dim(m2)) ){
    return(list('data' = d, 
              'raw_mat' = mat, 
              'dtm' = m3, 
              'adj_mat' = adj_mat))
  }else{
    return()
  }

}