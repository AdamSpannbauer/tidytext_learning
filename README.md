# tidy trump

Playing with fivethirtyeight trump tweet data to learn more about tidytext capabilities (and just to have some fun)

### data
    data(trump_twitter, package = "fivethirtyeight")

### libraries used
    library(tidytext)
    library(SnowballC)
    library(tidyverse)
    library(stringr)
    library(fivethirtyeight)
    library(lubridate)
    library(gridExtra)

### ggplot2 results
#### How does trump's sentiment change over time?
![Sentiment over time full](plots/sentOverTime.png)

#### What time of day does trump tweet the most?  Is sentiment more positive at certain times?
![Sentiment by hour](plots/sentByHour.png)

#### What day of week does trump tweet the most?  Is sentiment more positive on certain days?
![Sentiment by day](plots/sentByDay.png)

#### What month does trump tweet the most?  Is sentiment more positive in certain months?
![Sentiment over time full](plots/sentByMonth.png)
