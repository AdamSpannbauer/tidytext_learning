library(tidytext)
library(hunspell)
library(tidyverse)
library(stringr)
library(fivethirtyeight)
library(lubridate)
library(gridExtra)

data(trump_twitter, package = "fivethirtyeight")

# check out structure and date range ------------------------------------------------
str(trump_twitter)
(minDate <- min(date(trump_twitter$created_at)))
(maxDate <- max(date(trump_twitter$created_at)))

# create vectorised stemming function using hunspell ----------------------------------
my_hunspell_stem <- function(token) {
  stem_token <- hunspell_stem(token)[[1]]
  if (length(stem_token) == 0) return(token) else return(stem_token[1])
}
vec_hunspell_stem <- Vectorize(my_hunspell_stem, "token")

# clean text by tokenizing & rm urls/stopwords ----------------------------------
trump_tokens <- trump_twitter %>% 
  mutate(text = str_replace_all(text, 
                                pattern=regex("(www|https?[^\\s]+)"), 
                                replacement = "")) %>% #rm urls
  mutate(text = str_replace_all(text,
                                pattern = "[[:digit:]]",
                                replacement = "")) %>% 
  tidytext::unnest_tokens(tokens, text) %>% #tokenize
  mutate(tokens = vec_hunspell_stem(tokens)) %>% 
  filter(!(tokens %in% stop_words$word)) #rm stopwords

# get most used words in trumps tweets -----------------------------------------------
most_used_words <- trump_tokens %>% 
  group_by(tokens) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

# sentiment analysis -------------------------------------------------------------
sentimentDf <- get_sentiments("afinn") %>% 
  mutate(word = vec_hunspell_stem(word)) %>% 
  bind_rows(get_sentiments("afinn"))
trump_sentiment <- trump_tokens %>% 
  inner_join(sentimentDf, by=c("tokens"="word")) %>% 
  rename(Score=score)

trump_full_text_sent <- trump_sentiment %>% 
  group_by(id) %>% 
  summarise(Score = sum(Score)) %>% 
  ungroup() %>% 
  inner_join(trump_twitter, by="id")

# plot sentiment over time ------------------------------------------------------------
sentOverTimeGraph <- ggplot(data=trump_full_text_sent, aes(x=created_at, y=Score, colour=Score)) +
  geom_line() + 
  geom_point() +
  xlab("Date") +
  ylab("Sentiment (afinn)") +
  ggtitle(paste0("Trump Tweet Sentiment (",minDate," to ",maxDate,")"))

# examine top 5 most positive & most negative tweets --------------------------------
most_pos_trump <- trump_full_text_sent %>% 
  arrange(desc(Score)) %>% 
  head(n=3) %>% 
  .[["text"]]

catMostPos <- paste(collapse="\n\n\t", most_pos_trump)
cat("Most Positive Trump Tweets:\n\t",catMostPos, "\n")

most_neg_trump <- trump_full_text_sent %>% 
  arrange(Score) %>% 
  head(n=3) %>% 
  .[["text"]]

catMostNeg <- paste(collapse="\n\n\t", most_neg_trump)
cat("Most Negative Trump Tweets:\n\t",catMostNeg, "\n")

# when is trumps favorite time to tweet -------------------------------------------------
trump_tweet_times <- trump_full_text_sent %>% 
  mutate(weekday = wday(created_at, label=TRUE),
         month   = month(created_at, label=TRUE),
         hour    = strftime(created_at,format = "%H"))

plotSentByTime <- function(trump_tweet_times, timeGroupVar) {
  timeVar <- substitute(timeGroupVar)
  timeVarLabel <- stringr::str_to_title(timeVar)
  
  trump_tweet_time_sent <- trump_tweet_times %>% 
    rename_(timeGroup = timeVar) %>% 
    group_by(timeGroup) %>% 
    summarise(Score = sum(Score), Count = n()) %>% 
    ungroup()
  
  ggplot(trump_tweet_time_sent, aes(x=timeGroup, y=Count, fill = Score)) +
    geom_bar(stat="identity") +
    xlab(timeVarLabel) +
    ggtitle(paste("Trump Tweet Count & Sentiment by", timeVarLabel))
}

sentByHour  <- plotSentByTime(trump_tweet_times, hour)
sentByDay   <- plotSentByTime(trump_tweet_times, weekday)
sentByMonth <- plotSentByTime(trump_tweet_times, month)

# save plots ------------------------------------------------------------------
ggsave("plots/sentOverTime.png", plot=sentOverTimeGraph)
ggsave("plots/sentByHour.png", plot=sentByHour)
ggsave("plots/sentByDay.png", plot=sentByDay)
ggsave("plots/sentByMonth.png", plot=sentByMonth)

# plot everything ----------------------------------------------------------------
# grid.arrange(sentOverTimeGraph,
#              sentByHour,
#              sentByDay,
#              sentByMonth,
#              ncol=2)


