library(tidytext)
library(SnowballC)
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

# clean text by tokenizing & rm urls/stopwords ----------------------------------
trump_tokens <- trump_twitter %>% 
  mutate(text = str_replace_all(text, 
                                pattern=regex("(www|https?[^\\s]+)"), 
                                replacement = "")) %>% #rm urls
  tidytext::unnest_tokens(tokens, text) %>% #tokenize
  mutate(tokens = wordStem(tokens)) %>% 
  filter(!(tokens %in% stop_words$word)) #rm stopwords

# get most used words in trumps tweets ----------------------------------
most_used_words <- trump_tokens %>% 
  group_by(tokens) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

# sentiment analysis -------------------------------------------------------------
sentimentDf <- get_sentiments("afinn")
trump_sentiment <- trump_tokens %>% 
  left_join(sentimentDf, by=c("tokens"="word")) %>% 
  mutate(score = ifelse(is.na(score), 0, score)) %>% 
  rename(Score=score)

trump_full_text_sent <- trump_sentiment %>% 
  group_by(id) %>% 
  summarise(Score = sum(Score)) %>% 
  ungroup() %>% 
  right_join(trump_twitter, by="id")

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
  head(n=5) %>% 
  .[["text"]]

catMostPos <- paste(collapse="\n\n\t", most_pos_trump)
cat("Most Positive Trump Tweets:\n\t",catMostPos, "\n")

most_neg_trump <- trump_full_text_sent %>% 
  arrange(Score) %>% 
  head(n=5) %>% 
  .[["text"]]

catMostNeg <- paste(collapse="\n\n\t", most_neg_trump)
cat("Most Negative Trump Tweets:\n\t",catMostNeg, "\n")

# when is trumps favorite time to tweet -------------------------------------------------
trump_tweet_times <- trump_full_text_sent %>% 
  mutate(weekday = wday(created_at),
         month   = month(created_at),
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
ggsave("sentOverTime.png", plot=sentOverTimeGraph)
ggsave("sentByHour.png", plot=sentByHour)
ggsave("sentByDay.png", plot=sentByDay)
ggsave("sentByMonth.png", plot=sentByMonth)

# plot everything ----------------------------------------------------------------
# grid.arrange(sentOverTimeGraph,
#              sentByHour,
#              sentByDay,
#              sentByMonth,
#              ncol=2)


