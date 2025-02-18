load("company.rdata")
load("news.RData")

# Loading of packages -----------------------------------------------------

## Table manipulations
if (!require('data.table'))         install.packages('data.table')          ; library('data.table')
## Text processing
if (!require('quanteda'))           install.packages('quanteda')            ; library('quanteda')
## Quanteda add-on
if (!require('quanteda.textplots')) install.packages('quanteda.textplots')  ; library('quanteda.textplots')
## Quanteda add-on
if (!require('quanteda.textstats')) install.packages('quanteda.textstats')  ; library('quanteda.textstats')
## Topic modeling
if (!require('seededlda'))          install.packages('seededlda')           ; library('seededlda')
## POS
if (!require('spacyr'))             install.packages('spacyr')              ; library('spacyr')
## plotting
if (!require('ggplot2'))            install.packages('ggplot2')             ; library('ggplot2')

# Load associated functions ----------------------------------------------------------------

source("functions.R")

### Step1: Data description -----------------------------------------------------

#adding the lexisnexis relevance score
news <- cbind(news, relevance=NA)

for(i in 1:nrow(news)){
  df <- company[[i]]
  if(length(df)==2){
    value <- subset(df,df$X1=="KELLOGG CO")
    #print(as.numeric(as.character(value$X2)))
    test<- as.numeric(as.character(value$X2))
    if(length(test)==0){
      test <- 0
    }
    news[i,5]<- test[1]}else{news[i,5]<- 0}
  
}

#removing all the scores that had something wrong with them 
news2 <- subset(news,news$relevance!=0)
#news <- news[l.news > 200]

#finding the quantiles
quantile(news2$relevance)

#plotting a histogram of the scores
#this is to narrow down the articles that are most relevant in regards to the reputaiton of Kellogg
hist(news2$relevance, main= "Histogram of Relevancy Score", col="red")

### Preprocess data:
load("company.rdata")
l.news <- quanteda::ntoken(news$text, remove_punct = TRUE,
                           remove_numbers = TRUE, remove_url = TRUE) 
news <- news[l.news > 200 ,]

news<- news[news$pubTypes != "newswire", ] #subtract the newsire artcles out


#basic tokenization step
library('quanteda')
toks <- tokens(news$text, remove_punct = TRUE, remove_symbols = TRUE,
               remove_numbers = TRUE, remove_url = TRUE)
toks <- tokens_tolower(toks)

#finding compound words
bigrams <- quanteda.textstats::textstat_collocations(toks, size = 2, min_count = 500)

selected_bigrams <- c("per share","kellogg company","kellog co","market data", "nyse k", "special k", "rice krispies", "battle creek")
compound_pattern <- sapply(selected_bigrams, phrase)
head(compound_pattern)

toks2 <- tokens_compound(toks, compound_pattern, case_insensitive = TRUE)

toks2 <- tokens_wordstem(toks2)

dfm <- dfm(toks2)
textplot_wordcloud(dfm, max_words = 200)

library('seededlda')
dfm <- dfm_remove(dfm, stopwords("en", source = "marimo"))
textplot_wordcloud(dfm, max_words = 200)

dfm <- dfm_remove(dfm, c("also", "can", "inc", "news"))
dfm <- dfm_remove(dfm, min_nchar = 3)
dfm <- dfm_trim(dfm, min_termfreq = 50)


dfm<- dfm_trim(dfm, max_termfreq = 0.5*nrow(dfm))
ncol(dfm)
head(sort(colSums(dfm), decreasing = T), 10)

textplot_wordcloud(dfm, max_word = 200)

if (TRUE) { # simply replace FALSE by TRUE if you want to run the estimation
  system.time({
    set.seed(123)
    topicModel <- textmodel_lda(dfm, k = 10, max_iter = 2000, verbose = T,
                                alpha = .1, beta = 0.001)
  })
} else load("topicModel.Rdata") # if FALSE, it load the saved model


terms(topicModel, 25)

plotTerms(topicModel, 10)

mergin_list<- list( #1 2 3 4 5  6 7 8 9 10
  'Promotional activity' <- c(3,4), 
  'product launch' <- c(1),
  'recal' <- c(10),
  'Investment analysis'  <- c(5,6,7,8),
  'product claims' <- c(2,9)
)

mergedModel <- mergeTopics(topicModel, mergin_list)
plotTerms(mergedModel)
wordcloudTerms(mergedModel)

save(topicModel,file = "topicModel.Rdata")


### Step2: Computing sentiment ---------------------------

library('sentometrics')
lex <- sento_lexicons(lexiconsIn = list_lexicons["GI_en"], 
                      valenceIn = list_valence_shifters[["en"]])
sentiment <- compute_sentiment(news$text, lexicons = lex, how="UShaped")

data <- data.frame(
  id = 1:nrow(news),
  date = news$date,
  s_lexicon = sentiment$GI_en,
  as.data.frame(mergedModel$theta)
)
dim(data)
data[1:2,]

### STEP 3: Aggregate into Time series -------------------

library('xts')
# Creating an xts object with all the topics:
xts <- xts(data[, c("s_lexicon", "theme1", "theme2", "theme3", "theme4", "theme5")],
           as.Date(data$date))
head(xts)

## Total nr of documents per period:
apply.yearly(xts, nrow) |> plot(main="Total yearly news", col="red", type='o')
apply.weekly(xts, nrow) |> plot(main="Total weekly news", col="red")

weekly <- apply.weekly(xts, nrow)
diff_2 <- diff(weekly[,1])
plot(weekly)

## Create table that counts the nr of documents per origin:
library('data.table')
dt <- as.data.table(news)
dt$date <- as.Date(dt$date)

xts_dt <- dcast(dt, date ~ pubTypes, fun.aggregate = length, value.var = "date") |> 
  as.xts() |> 
  apply.yearly(colSums)

# Detrending the data: First-difference
diff1 = diff(xts_dt)
plot(diff1, main="Total news", legend.loc = "topleft", type='l')


library("pracma")
### Aggregating sentiment: WEEKLY
xts_sentiment <- xts(
  data$s_lexicon,
  as.Date(data$date))
xts_sentiment <- apply.weekly(xts_sentiment, colMeans)

sentiment_scale <- scale(xts_sentiment[,1])
article <- detrend(weekly[,1], tt = 'linear')


plot(article[,1])
plot(diff_2, main="Total news") #total nr of documents over time
plot(sentiment_scale[,1], col="red")

### STEP 5: SMOOTHING & PLOTTING -----------
xts_sentiment |> rollapply(width=5, fill=NA, FUN  = mean) |>
  plot()

article |> rollapply(width=10, fill=NA, FUN  = mean) |>
  plot(main="Smoothed Amount of Articles")

sentiment_scale|> rollapply(width=10, fill=NA, FUN  = mean) |>
  plot(main="Smoothed Sentiment Score")


## trigger_s highlights a possible reputation event trigger
# it is based on the total news and the associated sentiment score (weekly)
article_s <- article |> rollapply(width=10, fill=NA, FUN  = mean)
sentiment_s <-sentiment_scale|> rollapply(width=15, fill=NA, FUN  = mean)

Trigger_s <- sentiment_s*article_s
plot(Trigger_s)
abline(h=-0.5, col="red")

x2<- vec <- rep(-0.6, length(Trigger_s))
x3 <- merge(Trigger_s, x2, all = TRUE)

plot(x3, main = "Reputation event trigger")



### EXTRA:
# Apply a threshold for the number of observation required for a single week:
# threshold chosen arbitrary: 10
idx_threshold <- apply.weekly(xts, nrow) > 10

# Ensure regularly spaced data
index(idx_threshold) <- lubridate::ceiling_date(
  index(idx_threshold), unit = "week", week_start = 1)
head(idx_threshold)
idx_threshold2 <- as.logical(idx_threshold)
head(idx_threshold2)

## Visualizing the periods with enough observations
# Filled lines indicate the weeks that contain more than 5 articles
library(ggplot2)
ggplot(fortify(idx_threshold),
       aes(Index, as.numeric(idx_threshold))) + geom_col(width = 7, col='green') +
  scale_y_continuous(name = NULL, labels = NULL, breaks = NULL)

