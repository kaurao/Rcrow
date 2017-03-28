# a bunch of utilities to deal with twitter in R 
library(twitteR)
library(ROAuth)
library(httr)
library(plyr)

# authorize to get twitter access
# call this function once at the beginning of your session
# you need to register your api on twitter to get access
twitter_connect <- function(api_key, api_secret, access_token, access_token_secret) {
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
}

# Grab latest tweets
# Input
# string: string to search for (string)
# n     : number of tweets to retrieve (integer)
# get.text: whether to return tweet text (strip other information like author)
# Return
# If get.text is FASLSE then a list of tweets
# If get.text is TRUE then a list of tweet texts
get_tweets <- function(string, n, get.text=FALSE, ...) {
  tweets <- searchTwitter(string, n=n, ...)
  # Loop over tweets and extract text
  if(get.text) tweets <- lapply(tweets, function(tt) tt$getText())
  return(tweets)
}

# Filter tweets
# Input
# tweets: as returned by get_tweets with get.txt set to FALSE
# rm.retweet: remove retweets (TRUE/FALSE)
# author: only keep tweets from this author (string)
# Return
# A list of filtered tweets
filter_tweets <- function(tweets, rm.retweet=TRUE, author=NA) {
  tt <- rep(TRUE, length(tweets))
  for(i in 1:length(tweets)) {
    if(!is.na(author) && tweets[[i]]$screenName!=author) tt[i] <- FALSE
    #show(tweets[[i]]$screenName)
    if(rm.retweet && tweets[[i]]$retweeted) tt[[i]] <- FALSE
  }
  return(tweets[tt])
}

# A simple pretty print of tweets
# Input
# tweets: A list of tweets as returned by get_tweets or filter_tweets
# rm.retweet: whether to remove retweets (TRUE/FALSE)
# author: Print tweets only from this author (string)
# Return
# Print to the console
print_tweets <- function(tweets, rm.retweet=TRUE, author=NA) {
  for(i in 1:length(tweets)) {
    if(!is.na(author) && tweets[[i]]$screenName!=author) next
    if(rm.retweet && tweets[[i]]$retweeted) tt[[i]] <- next
    show(tweets[[i]]$screenName)
    show(tweets[[i]]$created)
    show(tweets[[i]]$text)
  }
}

# Get the text from the tweets (strips meta-data like author)
# Inout
# tweets: A list of tweets as returned by get_tweets with get.text=FALSE
# Return
# A list of text entries of the tweets
text_tweets <- function(tweets) {
  tweets <- lapply(tweets, function(tt) tt$getText())
  return(tweets)
}

# Search
# Input
# feeds: a list of strings, e.g. as returned by  text_tweets
# terms: a list of strings with the terms to search for in the feeds
# ignore.case: whether to make case-sensitive search (FALSE) or not (TRUE)
# Output
# A list with two elemsnts
# feeds: A text list that matches the search
# match: an integer array with matched index
search_list <- function(feeds, terms, ignore.case = TRUE, ...) {
    found <- list()
    for(tt in terms) {
      xx <- sapply(feeds, function(ff) 
            ifelse(length(grep(tt, ff, ignore.case=ignore.case, ...)),1,0))
      found[[tt]] <- which(xx==1)
    }
    return(list(feeds=feeds[unlist(found)], match=found))
}
