library(tuber)
library(ggplot2)
library(ggrepel)
library(data.table)
library(httr)
library(jsonlite)
library(plyr)
library(scales)
library(stringr)
library(rtweet)
library(dplyr)
library(igraph)
library(semnet)
# if you don't have this library, install it with:
# devtools::install_github("kasperwelbers/semnet")

# check that you have your token ready:
yt_oauth(app_id = "add_id_here", app_secret = "add_secret_here")

# check if you get anything here:
yt_token()

# and if this is TRUE
yt_authorized()

#### we first get 50 videos about trump
videos_trump <- yt_search("trump", type = "video", get_all=F)

# looks relevant - a lot of news channels:
videos_trump$channelTitle

# the function for channels stats in the tuber package is inefficient and costly
# here is a more efficient function that always gives us for 50 channels the stats:

yt_channel_stats <- function(ids, token=getOption("google_token"), part=c("snippet", "statistics")) {
  
  query = list(part = paste(part, collapse=","), 
               id=paste(ids, collapse=","), 
               maxResults = "50")
  
  test <- youtube_api(path="https://www.googleapis.com/youtube/v3/channels", 
                      token=token, 
                      query=query)
  
  if(test$response$status_code!=200) {
    warning( paste("error", test$response$status_code ) )
    return(paste("error", test$response$status_code ))
  }
  
  if(length(test[["content"]][["items"]]) == 0) {
    warning("0 results")
    return("0 results")
  }
  
  temp_df <- lapply(test$content$items, function(x) unlist(x$snippet))
  temp_df <- ldply(temp_df, rbind)
  temp_stats <- lapply(test$content$items, function(x) unlist(x$statistics))
  temp_stats <- ldply(temp_stats, rbind)
  temp_df <- cbind(temp_df, temp_stats)
  temp_df[, ] <- lapply(temp_df[, ], as.character)
  temp_df$id <- sapply(test$content$items, function(x) unlist(x$id))
  temp_df$etag <- sapply(test$content$items, function(x) unlist(x$etag))
  temp_df$viewCount <- as.numeric(temp_df$viewCount)
  temp_df$subscriberCount <- as.numeric(temp_df$subscriberCount)
  temp_df$videoCount <- as.numeric(temp_df$videoCount)
  temp_df
}

youtube_api <- function(path=path , token=token, query=query) {
  
  resp <- GET(path,
              config(token = token),
              query = query)
  
  
  if (http_type(resp) != "application/json") {
    stop(
      sprintf(
        "API did not return json: %s",
        http_status(resp)$message
      ),
      call. = FALSE
    )
  }
  
  parsed <- jsonlite::fromJSON(content(resp, as="text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
  
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "youtube_api"
  )
}

# now we can download all the stast for the videos
# with the following loop, you can get always all video ids
# here we loop through 50 batches of channel ids
# with ceiling we know how many rounds we need
# some might be duplicated, so we take only unique
channel_ids <- unique(videos_trump$channelId)

length_ids <- ceiling(length(channel_ids)/50)
channel_list <- list()


for( i in 1:length_ids) {
  if(i != length_ids) {
    channel_list[[i]] <- try(yt_channel_stats(ids=channel_ids[(i*50-49):(i*50)]), silent = T)
  } else {
    channel_list[[i]] <- try(yt_channel_stats(ids=channel_ids[(i*50-49):length(channel_ids)]), silent = T)
  }
}

channels_trump_stats <- do.call(rbind, channel_list)

str(channels_trump_stats)

# here the stats are already numeric as I have changed it already in the yt_channel_stats function

# we want to order them by video count as there are too many videos...
channels_trump_stats <- channels_trump_stats[order(channels_trump_stats$videoCount), ]
channels_trump_stats$videoCount
# now we want all the videos of these channels:

# now we analyze the first 10 channels
# we can be really efficient by getting all the video ids over playlists
# don't use the get_all_channel_video_stats function, it will cost you too much
channels_trump_stats$id[1:10]
# this will take a few minutes
# we can always get the last 20k videos max

# the playlist ids just have UU instead of UC, we replace them
playlist_ids <- channels_trump_stats$id[1:10]
str_replace(playlist_ids, "UC", "UU")
playlist_ids <- str_replace(playlist_ids, "UC", "UU")

# we create a loop for that:
play_list <- list()
for( i in 1:length(playlist_ids) ) {
  play_list[[i]] <- get_playlist_items(filter = c(playlist_id = playlist_ids[i]), max_results = 100)
  message("finished with ", i)
}

# we merge the data frames in the list to a single data frame
play_df <- do.call(rbind, play_list)
# we can now just create one single vector by selecting the video id colum:
video_ids <- play_df$contentDetails.videoId
length(video_ids)
length(video_ids)/50

# now we use my efficient video function that always takes 50 videos at the same time:
get_video_details_all <- function(video_ids = NULL, part = "snippet", as.data.frame = FALSE, ...) {
  
  results <- list()
  n_loop <- ceiling(length(video_ids)/50)
  
  for(i in 1:n_loop) {
    if(i == n_loop) {
      results[[i]] <- get_video_details(video_id = video_ids[(i*50-49):length(video_ids)], as.data.frame = as.data.frame,  part = part)
    } else {
      results[[i]] <- get_video_details(video_id = video_ids[(i*50-49):(i*50)], as.data.frame = as.data.frame,  part = part)
    }
    message("finished with ", i, "/",n_loop)
  }
  return(results)
}

all_videos_df <- get_video_details_all(video_ids = video_ids, as.data.frame = T, part = c("snippet","statistics"))
all_videos_df <- rbindlist(all_videos_df, fill = T)

# now we can extract all the tags, we need one vector for each video
tag_list <- lapply(all_videos_df$tags, unlist)
# we remove empty tag lists
tag_list <- tag_list[!sapply(tag_list, is.null)]

# we want unique tags only
tag_list <- lapply(tag_list, unique)

# we also can chekc the most used tags:
tags_table <- as.data.frame(table(unlist(tag_list)))
tags_table <- tags_table[order(tags_table$Freq, decreasing = T), ]
head(tags_table, 50)

# now we get the co-occurrences of the tags:
tag_pairs <- list()
for( i in 1:length(tag_list)) {
  # if only one hashtag, jump to next
  if(length(tag_list[[i]])<2) {
    next
  }
  tag_pairs[[i]] <- as.data.frame(t(combn(tag_list[[i]], m=2)), stringsAsFactors = F)
}


co_edges_tags <- as.data.frame( rbindlist(tag_pairs), stringsAsFactors=F )
colnames(co_edges_tags) <- c("source","target")

# we only analyze the top 200 tags - you can change that later
co_edges_tags <- co_edges_tags[co_edges_tags$source %in% tags_table$Var1[1:200] & co_edges_tags$target %in% tags_table$Var1[1:200], ]

# create a graph object
# use directed = TRUE if we have directed data
# co-occurrence networks are always undirected
tag_network <- graph_from_data_frame(co_edges_tags, directed = F)
# the function E shows the EDGES of the graph
E(tag_network)$weight
# we need to add weights as some of the edges have appear more than once
tag_network <- graph.adjacency(get.adjacency(tag_network), weighted=TRUE, mode = "undirected")
tag_network
# now we have the edge weights
E(tag_network)$weight

# as we have a very dense network, we have to extract the so-called backbone network:
# extract backbone
tag_backbone <- getBackboneNetwork(tag_network, alpha = 0.05, direction = "none",
                                    delete.isolates = T, max.vertices = NULL, use.original.alpha = T,
                                    k.is.Nvertices = F)
# network became smaller
tag_backbone
tag_network

# here a direct comparison
plot(tag_backbone)
plot(tag_network)

# now we can identify communities:
# community detection with Louvain
V(tag_backbone)$community <- membership(cluster_louvain(tag_backbone)) 

# we could check what tags occure often together:
table(V(tag_backbone)$community)

V(tag_backbone)$name[V(tag_backbone)$community == 1]
V(tag_backbone)$name[V(tag_backbone)$community == 2]
V(tag_backbone)$name[V(tag_backbone)$community == 3]
# ... etc.

# and now we can directly export it as a file for gephi
write_graph(tag_backbone, "tags.graphml", format="graphml")







