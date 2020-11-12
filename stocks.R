install.packages("textdata")
install.packages("prophet")
install.packages("forecast")
install.packages("tseries")
install.packages("quantmod")
install.packages("derivmkts")

library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(lubridate)
library(data.table)

#API and json
library(httr)
library(jsonlite)
library(config)

#Web Scraping
library(rvest)

#Visualization
library(plotly)
library(ggplot2)
library(DT)
library(tibble)

#Data
library(devtools)
library(gtrendsR)

#Text Analysis
library(tidytext)
library(wordcloud)
library(RColorBrewer)

#Forecasting
require(quantmod)
require(derivmkts)
library(forecast)
library(tseries)
library(prophet)

library(knitr)

## Data

#To obtain historical stock data, the World Trading Data API is used. This report allows for the selection of any stock for analysis by changing the ticker saved to the 'stock' variable. In this case SENSEX (S&P BSE SENSEX) data is used.
#After which data tidying is done.

t1<- ISOdate(2019,09,11,hour = 0)
as.integer(t1)

t2<- ISOdate(2020,09,11, hour = 0)
as.integer(t2)

stock<- "^BSESN"

url <- paste("https://query1.finance.yahoo.com/v7/finance/download/",
             
             stock,
             
             "?period1=",
             
             as.integer(t1),
             
             "&period2=",
             
             as.integer(t2),
             
             "&interval=1d&events=history",
             
             sep="")

dataset <- read.csv(url)
str(dataset)

dataset2 <- read.csv("india-news-headlines.csv")

## Price and Volume visualization

#Now that the stock dataset is tidied, a visualization of the price and volume time series is created. Plotly is used for this visualization.

stockdata <- dataset
stockdata$Name <- c("SENSEX")

p1 <- stockdata %>%
  plot_ly(x = ~Date,
          type = "candlestick", 
          open = ~Open, 
          close = ~Close, 
          high = ~High,
          low = ~Low,
          name = "price") %>%
  layout(rangeslider=list(visible = FALSE), yaxis = list(title = "Price ($)",
                                                         showgrid = TRUE,
                                                         showticklabels = TRUE))

p2 <- stockdata %>%
  plot_ly(x=~Date, y=~Volume, type='bar', Name = "Volume") %>%
  layout(yaxis = list(title = "Volume"))

p <- subplot(p1, p2, heights = c(0.7,0.3), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste0(stock))
p

## Google Trends

#Could Google search interest also be an indicator for srock prices? Using the gtrendsr package, the following code queries interest over time for the selected stock over the previous years and creates a visualization using plotly.

data("countries")

trends <- gtrends(keyword = stock, geo = "IN", onlyInterest = TRUE)
trends <- trends$interest_over_time %>%
  as_data_frame() %>%
  select(c(date, hits, keyword))

trends$date <- as_date(ceiling_date(trends$date, unit = "weeks", change_on_boundary = NULL, week_start = getOption("lubridate.week.start",1)))

trends %>%
  plot_ly(x=~date, y=~hits, mode= 'lines', name = "Google Search Trends") %>%
  layout(title= paste0("Interest over time:",stock), yaxis = list(title= "hits"))

## Relation between interest and stock price

#Using the google trends dataset, it is now possible to view the relationship between interest over time ('hits') and stock performance. To do this, a left join is used to combine trend and stock data by date. The outcome of the join is then used to plot the relationship between hits and stock close price.

trends<- rename(trends, Date= date)
stockdata$Date <- as_date(stockdata$Date)
trends %>%
  left_join(stockdata, by= "Date") %>%
  select(one_of(c("Date", "hits", "Close"))) %>%
  drop_na() %>%
  ggplot(aes(hits, Close))+
  geom_point(color= "blue")+
  geom_smooth(method = lm, color= "black") +
  labs(title =paste0(stock,": Relationship between Hits and Close Stock Price"))

## Sentimental analysis

#News articles provide excellent insight on the performance of each stock. The next step in this stock performance report is to import and perform sentiment analysis on recent news articles about the stock.
#The first step is to unnest each word in the article description, allowing for a 'bag of words' sentiment analysis approach. For a quick visualization of the most frequently used words, a word cloud is created.

#To perform basic sentiment analysis, the afinn sentiment lexicon is used. This lexicon assigns scores to each word on a scale of -5 to 5. To view news sentiment about the selected company over the past month, the dataset is grouped by article and date and the score is summarised by the mean for each group.

newsart <- dataset2

newsart<-newsart %>%
  filter(str_detect(headline_text,'SENSEX|Sensex|sensex'))

newsart <- transform(newsart, publish_date = as.Date(as.character(publish_date), "%Y%m%d"))

newsart<- newsart[-c(4)]

newsart<-newsart %>%
  filter(publish_date>'2019-09-10')
newsart<-newsart %>%
  filter(publish_date<'2020-09-11')

news_words <- newsart %>%
  select(c("publish_date","headline_category","headline_text"))%>%
  unnest_tokens(word, headline_text) %>%
  filter(!word %in% append(stop_words$word, values = "chars"), str_detect(word, "^[a-z']+$"))

news_words$publish_date = as_date(news_words$publish_date)

words_only<- news_words %>%
  count(word, sort = TRUE)

set.seed(1)
wordcloud(words = words_only$word, freq = words_only$n, scale = c(5,.5), max.words = 50, colors = brewer.pal(8, "Dark2"))

afinn<- get_sentiments("afinn")

sentiment_summary <- news_words %>%
  left_join(afinn) %>%
  filter(!is.na(value)) %>%
  group_by(headline_category, publish_date) %>%
  summarise(value= mean(value)) %>%
  mutate(sentiment= ifelse(value>0, "positive", "negative"))

datatable(sentiment_summary)

ggplot(sentiment_summary, aes(publish_date, value)) + geom_bar(stat = "identity", aes(fill=sentiment))  + ggtitle(paste0(stock, ": News Sentiment Over Time"))

## Forecasting

#In the previous steps, various factors such as news sentiment and Google trends were analyzed. In this step, the Prophet API will be used to forecast future prices for the selected stock.
#In this next step, the data is pre-processed to fit the requirements of Prophet and a prediction is created (accounting for the regular stock gaps on weekends). The output of the forecast is the date, forecasted close price, and the lower and upper confidence intervals based on an 80% confidence levels.
#The results of the time series forecasting are plotted below. For most stocks, it seems like Prophet was able to capture the trends in close prices, but fails to forecast sharp changes in price.
#To further evaluate the forecast results, a residual plot is created. Based on the residual plot, it is evident that this forecast does not capture all of the variability in stock prices over time.

df<- stockdata %>%
  select(c("Date", "Close")) %>%
  rename(ds= Date, y= Close)

df<-df %>%
  filter(y!='null')

m <- prophet(df)

future <- make_future_dataframe(m, periods = 365) %>% filter(!wday(ds) %in% c(1,7))
forecast <- predict(m, future)
datatable(forecast[c('ds','yhat','yhat_lower','yhat_upper')])

plot(m, forecast, xlabel = "date", ylabel = "stock close price ($)") + ggtitle(paste0(stock, ": Stock Price Prediction"))

#Forecast Evaluation

forecast$ds <- as_date(forecast$ds)

is.numeric(df$y)
is.numeric(forecast$yhat)

df$y<- as.numeric(df$y)
is.numeric(df$y)

residuals <- df %>% 
  left_join(forecast[c('ds','yhat','yhat_lower','yhat_upper')], by = "ds") %>%
  filter(ds < today()) %>%
  mutate(res = (y-yhat))

datatable(residuals)

ggplot(residuals, aes(ds, res)) + geom_point() + geom_hline(yintercept =0, color = "red") + labs(title ="Prophet Forecasting Residuals", x = "date", y = "residual")
