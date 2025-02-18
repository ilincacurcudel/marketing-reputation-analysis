load("company.rdata")
load("news.RData")


# Loading of packages -----------------------------------------------------

## Table manipulations
if (!require('data.table'))         install.packages('data.table')          ; library('data.table')
## Text processingx
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
## plotting
if (!require('ggwordcloud'))        install.packages('ggwordcloud')         ; library('ggwordcloud')
## plotting
if (!require('xts'))        install.packages('xts')         ; library('xts')
## plotting
if (!require('quantmod'))        install.packages('quantmod')         ; library('quantmod')
## plotting
if (!require('lmtest'))        install.packages('lmtest')         ; library('lmtest')
## plotting
if (!require('ROCR'))        install.packages('ROCR')         ; library('ROCR')
## plotting
if (!require('gridExtra'))        install.packages('gridExtra')         ; library('gridExtra')
## plotting
if (!require('cowplot'))        install.packages('cowplot')         ; library('cowplot')
## plotting
if (!require('sentometrics'))        install.packages('sentometrics')         ; library('sentometrics')


### Load associated functions ----------------------------------------------------------------
source("functions.R")


### Step1: Data  & LDA estimation -----------------------------------------------------

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
  'Promotional activity' = c(3,4), 
  'Product launch'= c(1),
  'Product recal'= c(10),
  'Investment analysis'= c(5,6,7,8),
  'Product claims' = c(2,9)
)

mergedModel <- mergeTopics(topicModel, mergin_list)
plotTerms(mergedModel)
wordcloudTerms(mergedModel)

save(topicModel,file = "mergedModel.Rdata")


## SANKEY diagram -------------
df2 <- data.frame(
  x = c(1, 1, 2, 2, 2, 3, 3),
  node = as.factor(c("Initial data", "Initial data", "Words>200", "Words>200", "Discarded", "Relevant sources","Discarded")),
  next_x = c(2, 2, 3, 3, 3, NA, NA),
  next_node = c("Words>200", "Discarded", "Relevant sources", "Discarded", "Discarded", NA, NA),
  value = c(5437, 3064, 3574, 1863, 3064, 3574, 4927)
)

library(ggplot2)
library(dplyr)
ggplot(df2, aes(x = x, next_x = next_x, node = node,
                next_node = next_node, fill = node,
                value = value, label = node)) + 
  geom_sankey(flow.fill = "black", flow.alpha = .2, width = .4) +
  theme_sankey() +
  geom_sankey_label() + guides(fill = "none")


### Step2: Compute sentiment ---------------------------

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


### Load topic model --------------------------------------------------------
load("mergedModel.Rdata") # this is topicModel
load("topicModel.Rdata") #this is mergedModel


##### Step 3: Aggregating time series & smoothing: -----------------

## 3.1. Sentiment time series -------------------
library('xts')

xts_sentiment <- xts(
  data$s_lexicon,
  as.Date(data$date))

xts_sentiment <- apply.weekly(xts_sentiment, colMeans)

## apply rolling average:
xts_sentiment_smoothed <- rollapply(xts_sentiment, width=15, FUN  = mean)

## scale the series
xts_sentiment_scale <- scale(xts_sentiment[,1])
xts_sentiment_smoothed1 <- scale(xts_sentiment_smoothed[,1])

plot(xts_sentiment_smoothed)


## 3.2. Total number of articles------------------------
library('xts')
# Creating an xts object with all the topics:
xts_articles <- xts(data[, c("s_lexicon", "Promotional.activity", "Product.launch", "Product.recal", "Investment.analysis", "Product.claims")],
           as.Date(data$date))

## Total nr of documents per period:
apply.yearly(xts_articles, nrow) |> plot(main="Total yearly news", col="red", type='o')
apply.weekly(xts_articles, nrow) |> plot(main="Total weekly news", col="red")
articles <- apply.weekly(xts_articles, nrow)

library(forecast)
articles_detrended <- tsclean(articles[,1])

## apply rolling average  & scaling:
articles_smoothed <- rollapply(articles, width=10, FUN  = mean)

# PLOT(s):
plot(articles_detrended, main="Total news") #total nr of documents over time
apply.yearly(xts_articles, nrow) |> plot(main="Total yearly news", col="red", type='o')


### 3.3: SMOOTHING & PLOTTING -------
xts_sentiment |> rollapply(width=5, fill=NA, FUN  = mean) |>
  plot()

article_detrended |> rollapply(width=10, fill=NA, FUN  = mean) |>
  plot(main="Smoothed Amount of Articles")

sentiment_scale|> rollapply(width=10, fill=NA, FUN  = mean) |>
  plot(main="Smoothed Sentiment Score", color="red")




### Step 4: TRIGGER signal feature -----------------

library('xts')
# Creating an xts object with all the topics:
xts_articles <- xts(data[, c("s_lexicon", "Promotional.activity", "Product.launch", "Product.recal", "Investment.analysis", "Product.claims")],
           as.Date(data$date))
head(xts_articles)


library("pracma")
### Aggregating sentiment: WEEKLY
xts_sentiment <- xts(
  data$s_lexicon,
  as.Date(data$date))
xts_sentiment |> apply.weekly(xts_sentiment, colMeans) 

plot(xts_sentiment, color="red", main="Aggregated sentiment score")
plot(article_detrended, main="Aggregated number of articles")

sentiment_scale <- scale(xts_sentiment[,1])
article_detrended <- detrend( articles[,1], tt = 'linear')

## trigger_s highlights a possible reputation event trigger
# it is based on the total news and the associated sentiment score (weekly)
article_smooth <- article_detrended |> rollapply(width=10, fill=NA, FUN  = mean)
sentiment_smoothed <-sentiment_scale|> rollapply(width=15, fill=NA, FUN  = mean)

Trigger_s <- sentiment_smoothed*article_smooth
plot(Trigger_s)
abline(h=-0.5, col="red")


x2<- vec <- rep(0, 894)
x3 <- merge(Trigger_s, x2, all = TRUE)

plot(x3, main = "Reputation event trigger")





#### Step 5: EVALUATION  ---------------------------

### 5.1:  Binary event variable --------

# # #fiLL the NAs with the mean:
# mean_value <- mean(Trigger_s[,1], na.rm = TRUE)
# Trigger_s[is.na(Trigger_s[,1])] <- mean_value

Event <- rep(0,894)
Place_holder <- merge(Trigger_s,Event)
Event <- xts(Place_holder$Event)

Event["2000-11-25",]<-1 #glas in the box 
Event["2005-05-27",]<-1 #Death of Tony
Event["2009-04-26",]<-1 #FTC settlement
Event["2010-05-28",]<-1 #2010 Recall bcs of a chemical 
Event["2012-06-09",]<-1 #52-week low for the stock
Event["2012-07-06",]<-1 #SpecialK bad advertisment about weight loss
Event["2013-07-07",]<-1 #Pulling 2 advertismements down bcs of obesity claims about one of their products
Event["2016-04-17",]<-1 #Job layoffs 
Event["2016-11-12",]<-1 #Amnesty situation

plot(Event, main="Events", color="green")
hist(Event)
table(Event)
table(as.numeric(Event))

#events that picked up the signal given the cutoff threshold: 2012-07-06, 2010-05-28, 2000-11-25 



### general dataframe ----------------------
full_weekly <- apply.weekly(xts_articles, colMeans)
full_weekly$sentiment <- scale(full_weekly$s_lexicon)
full_weekly$articles <- detrend(articles[,1], tt = 'linear')
full_weekly$trigger <- full_weekly$articles*full_weekly$s_lexicon
full_weekly$sentiment_s <-full_weekly$s_lexicon|> rollapply(width=10, fill=NA, FUN  = mean)
full_weekly$articles_s <-full_weekly$articles|> rollapply(width=10, fill=NA, FUN  = mean)
full_weekly$event <- Event


####### 5.2: TRAIN THE GLM model + plot ROCR curve (for each model) -----------------------

## MODEL 1: Trigger scores ------------------

model_data1<- merge(Event, Trigger_s)
model_data1 <- na.omit(model_data1)

#install.packages("caret")
library(caret)
set.seed(123)
Smodel <- glm(Event ~ Trigger_s, model_data1, family = binomial())
summary(model)

# predictions:
predictions <- predict(model, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.01, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data1$Event, levels = c(0, 1))

metrics <- confusionMatrix(actuals_factored, predictions_factored)
metrics
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index <- TPR + FPR - 1
Youden_index #the model seems to show very high accuracy in classifying positive and negative cases correctly.


## ROCR for this model:
# Create a prediction object for ROCR
pred_obj1 <- prediction(predictions, model_data1$Event)

# Calculate the ROC curve
roc_curve1 <- performance(pred_obj1, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve1, col="red")


## MODEL 2: Sentiment score -------------------------
model_data2 <- merge(Event, xts_sentiment)

#install.packages("caret")
library(caret)
set.seed(123)
model2 <- glm(Event ~ xts_sentiment, model_data2, family = binomial())
summary(model2)

# predictions:
predictions <- predict(model2, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.01, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data2$Event, levels = c(0, 1))

metrics2 <- confusionMatrix(actuals_factored, predictions_factored)
metrics2
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR2 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR2 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index2 <- TPR2 + FPR2 - 1
Youden_index2 #the model seems to show very high accuracy in classifying positive and negative cases correctly.

## ROCR for this model:

# Create a prediction object for ROCR
pred_obj2 <- prediction(predictions, model_data2$Event)

# Calculate the ROC curve
roc_curve2 <- performance(pred_obj2, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve2, col="red")


## MODEL 3: Smoothed Trigger----------------

Trigger_smooth <- Trigger_s |> rollapply(width=10, fill=NA, FUN  = mean) 

model_data3 <- merge(Event, Trigger_smooth)
model_data3 <- na.omit(model_data3)

#install.packages("caret")
library(caret)
set.seed(123)
model3 <- glm(Event ~  Trigger_smooth, model_data3, family = binomial())
summary(model3)

# predictions:
predictions <- predict(model3, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.01, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data3$Event, levels = c(0, 1))

metrics3 <- confusionMatrix(actuals_factored, predictions_factored)
metrics3
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR3 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR3 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index3 <- TPR3 + FPR3 - 1
Youden_index3 #the model seems to show very high accuracy in classifying positive and negative cases correctly.

### ROCR for this model:
# Create a prediction object for ROCR
pred_obj3 <- prediction(predictions, model_data3$Event)

# Calculate the ROC curve
roc_curve3 <- performance(pred_obj3, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve3, col="red")


## MODEL 4: Smmoothed sentiment ------------------------
sentiment_smoothed <-sentiment_scale|> rollapply(width=15, fill=NA, FUN  = mean)

model_data4 <- merge(Event, sentiment_smoothed)
model_data4 <- na.omit(model_data4)

#install.packages("caret")
library(caret)
set.seed(123)
model4 <- glm(Event ~ sentiment_smoothed, model_data4, family = binomial())
summary(model4)

# predictions:
predictions <- predict(model4, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.013, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data4$Event, levels = c(0, 1))

metrics4 <- confusionMatrix(actuals_factored, predictions_factored)
metrics4
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR4 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR4 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index4 <- TPR4 + FPR4 - 1
Youden_index4 #the model seems to show very high accuracy in classifying positive and negative cases correctly.

### ROCR for this model:
# Create a prediction object for ROCR
pred_obj4 <- prediction(predictions, model_data4$Event)

# Calculate the ROC curve
roc_curve4 <- performance(pred_obj4, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve4, col="red")



## MODEL 5: Detrended articles--------------------

model_data5 <- merge(Event, articles_detrended)

#install.packages("caret")
library(caret)
set.seed(123)
model5 <- glm(Event ~ articles_detrended, model_data5, family = binomial())
summary(model5)

# predictions:
predictions <- predict(model5, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.011, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data5$Event, levels = c(0, 1))

metrics5 <- confusionMatrix(actuals_factored, predictions_factored)
metrics5
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR5 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR5 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index5 <- TPR5 + FPR5 - 1
Youden_index5 #the model seems to show very high accuracy in classifying positive and negative cases correctly.


### ROCR for this model:
# Create a prediction object for ROCR
pred_obj5 <- prediction(predictions, model_data5$Event)

# Calculate the ROC curve
roc_curve5 <- performance(pred_obj5, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve5, col="red")


## MODEL 6: Smoothed articles------------------

article_smoothed <- article_detrended |> rollapply(width=10, fill=NA, FUN  = mean)

model_data6 <- merge(Event, article_smoothed)
model_data6 <- na.omit(model_data6)

#install.packages("caret")
library(caret)
set.seed(123)
model6 <- glm(Event ~ article_smoothed, model_data6, family = binomial())
summary(model6)

# predictions:
predictions <- predict(model6, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.010, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data6$Event, levels = c(0, 1))

metrics6 <- confusionMatrix(actuals_factored, predictions_factored)
metrics6
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR6 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR6 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index6 <- TPR6 + FPR6 - 1
Youden_index6 #the model seems to show very high accuracy in classifying positive and negative cases correctly.

### ROCR for this model:
# Create a prediction object for ROCR
pred_obj6 <- prediction(predictions, model_data6$Event)

# Calculate the ROC curve
roc_curve6 <- performance(pred_obj6, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve6, col="red")


## MODEL 7: Detrended articles & sentiment score --------

model_data7 <- merge(Event, xts_sentiment, articles_detrended)

#install.packages("caret")
library(caret)
set.seed(123)
model7 <- glm(Event ~ xts_sentiment + articles_detrended, model_data7, family = binomial())
summary(model7)

# predictions:
predictions <- predict(model7, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.015, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data7$Event, levels = c(0, 1))

metrics7 <- confusionMatrix(actuals_factored, predictions_factored)
metrics7
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR7 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR7 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index7 <- TPR7 + FPR7 - 1
Youden_index7 #the model seems to show very high accuracy in classifying positive and negative cases correctly.

## ROCR for this model:

# Create a prediction object for ROCR
pred_obj7 <- prediction(predictions, model_data7$Event)

# Calculate the ROC curve
roc_curve7 <- performance(pred_obj7, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve7, col="red")


## Model 8: Detrended articles & smoothed sentiment score -------------------
sentiment_smoothed <-sentiment_scale|> rollapply(width=15, fill=NA, FUN  = mean)

model_data8 <- merge(Event,articles_detrended, sentiment_smoothed)
model_data8 <- na.omit(model_data8)

#install.packages("caret")
library(caret)
set.seed(123)
model8 <- glm(Event ~ articles_detrended + sentiment_smoothed, model_data8, family = binomial())
summary(model8)

# predictions:
predictions <- predict(model8, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.015, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data8$Event, levels = c(0, 1))

metrics8 <- confusionMatrix(actuals_factored, predictions_factored)
metrics8
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR8 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR8 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index8 <- TPR8 + FPR8 - 1
Youden_index8 #the model seems to show very high accuracy in classifying positive and negative cases correctly.

## ROCR for this model:

# Create a prediction object for ROCR
pred_obj8 <- prediction(predictions, model_data8$Event)

# Calculate the ROC curve
roc_curve8 <- performance(pred_obj8, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve8, col="red")



## Model 9: Smoothed articles & sentiment score ---------------------

article_smoothed <- article_detrended |> rollapply(width=10, fill=NA, FUN  = mean)
model_data9 <- merge(Event, xts_sentiment,  article_smoothed)
model_data9 <- na.omit(model_data9)

#install.packages("caret")
library(caret)
set.seed(123)
model9 <- glm(Event ~ article_smoothed + xts_sentiment, model_data9, family = binomial())
summary(model9)

# predictions:
predictions <- predict(model9, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.011, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data9$Event, levels = c(0, 1))

metrics9 <- confusionMatrix(actuals_factored, predictions_factored)
metrics9
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR9 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR9 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index9 <- TPR9 + FPR9 - 1
Youden_index8 #the model seems to show very high accuracy in classifying positive and negative cases correctly.

## ROCR for this model:

# Create a prediction object for ROCR
pred_obj9 <- prediction(predictions, model_data9$Event)

# Calculate the ROC curve
roc_curve9 <- performance(pred_obj9, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve9, col="red")


## Model 10: Smoothed articles & smoothed sentiment score ------------
article_smoothed <- article_detrended |> rollapply(width=10, fill=NA, FUN  = mean)
sentiment_smoothed <-sentiment_scale|> rollapply(width=15, fill=NA, FUN  = mean)

model_data10 <- merge(Event, article_smoothed, sentiment_smoothed)
model_data10 <- na.omit(model_data10)

#install.packages("caret")
library(caret)
set.seed(123)
model10 <- glm(Event ~ article_smoothed + sentiment_smoothed, model_data10, family = binomial())
summary(model10)

# predictions:
predictions <- predict(model10, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.017, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data10$Event, levels = c(0, 1))

metrics10 <- confusionMatrix(actuals_factored, predictions_factored)
metrics10
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR10 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR10 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index10 <- TPR10 + FPR10 - 1
Youden_index10 #the model seems to show very high accuracy in classifying positive and negative cases correctly.

## ROCR for this model:

# Create a prediction object for ROCR
pred_obj10 <- prediction(predictions, model_data10$Event)

# Calculate the ROC curve
roc_curve10 <- performance(pred_obj10, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve10, col="red")

## MODEL 11: Detrended articles & sentiment score& THEMES --------------

model_data11 <- full_weekly
model_data11 <- na.omit(model_data11)

#install.packages("caret")
library(caret)
model11 <- glm(event ~ articles+ sentiment+ Product.launch +Promotional.activity + Product.recal+ Investment.analysis+
                 Product.claims, model_data11, family = binomial())
summary(model11)

# predictions:
predictions <- predict(model11, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.017, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data11$event, levels = c(0, 1))

metrics11 <- confusionMatrix(actuals_factored, predictions_factored)
metrics11
#confusion_matrix <- table(predictions_factored, actuals_factored) 

# CALCULATE YOUDEN INDEX:
TPR11 <- sensitivity(actuals_factored, predictions_factored)

# False Positive Rate (FPR)
FPR11 <- 1 - specificity(actuals_factored, predictions_factored)

Youden_index11 <- TPR11 + FPR11 - 1
Youden_index11 #the model seems to show very high accuracy in classifying positive and negative cases correctly.

## ROCR for this model:

# Create a prediction object for ROCR
pred_obj11 <- prediction(predictions, model_data11$event)

# Calculate the ROC curve
roc_curve11 <- performance(pred_obj11, "tpr", "fpr")

# Plot the ROC curve
plot(roc_curve11, col="red")



### OPTIMAL CUTOFF: MODEL 11-----------------

# Define cutoff function:
best_cutoff<- function(perf){
  TN <- perf@tn[[1]]
  FN <- perf@fn[[1]]
  TP <- perf@tp[[1]] 
  FP <- perf@fp[[1]]
  beta <- 2
  Fscore <- ((1+(beta^2))*TP)/((1+(beta^2))*TP + FP + (beta^2)*FN)
  Cutoff<- perf@cutoffs[[1]][which.max(Fscore)]
  dt <- data.frame(matrix(c(1, 2), nrow = 1, ncol = 2))
  colnames(dt) <- c("Fscore", "Cutoff")
  dt[1,2]<- Cutoff
  dt[1,1]<- max(Fscore)
  return(dt)
}

# Cutoffs for each models:
dt1<- best_cutoff(pred_obj1)
dt2<- best_cutoff(pred_obj2)
dt3<- best_cutoff(pred_obj3)
dt4<- best_cutoff(pred_obj4)
dt5<- best_cutoff(pred_obj5)
dt6<- best_cutoff(pred_obj6)
dt7<- best_cutoff(pred_obj7)
dt8<- best_cutoff(pred_obj8)
dt9<- best_cutoff(pred_obj9)
dt10<- best_cutoff(pred_obj10)
dt11<- best_cutoff(pred_obj11)

dtt <- rbind(dt1,dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10,dt11)
dtt

#it seems that model 11 yields the highest Fscore

#Compute confusion matrix for model 11, now with new threshold: 0.01905980
model_data11 <- full_weekly
model_data11 <- na.omit(model_data11)

#install.packages("caret")
library(caret)
model11 <- glm(event ~ articles+ sentiment+ Product.launch +Promotional.activity + Product.recal+ Investment.analysis+
                 Product.claims, model_data11, family = binomial())
summary(model11)

# predictions:
predictions <- predict(model11, type = "response")
summary(predictions)
hist(predictions)
predictions_threshold <- ifelse(predictions > 0.01905980, 1, 0) #threshold is chosen arbitrary and updated 

#confusion matrix:
predictions_factored <- factor(predictions_threshold, levels = c(0, 1))
actuals_factored <- factor(model_data11$event, levels = c(0, 1))

metrics12 <- confusionMatrix(actuals_factored, predictions_factored)
metrics12 #the results are also displayed in the report


### PLOTs triggered events -----------------

# trigger 2000 
library(xts)
end_date <- as.Date("2000-11-25")
half_year_before <- end_date - 180
a_subset2000 <- window(a, end = end_date, start = half_year_before)


#trigger event 2010
end_date <- as.Date("2010-05-28")
half_year_before <- end_date - 180
a_subset2010 <- window(a, end = end_date, start = half_year_before)

## TRIGGER 2000 plot
library('ggthemes')
library(ggplot2)
library(xts)
ggplot(data.frame(date=index(a_subset2000), value=coredata(a_subset2000))) + 
  geom_line(aes(x=date, y=value), color="blue", size=1.2, alpha=0.5) + xlab("") + ylab("")+
  geom_hline(yintercept = 0.01905980, color = "red") + theme_gdocs()
 

## TRIGGER events plot
library('ggthemes')
library(ggplot2)
library(xts)
ggplot(data.frame(date=index(a_subset2010), value=coredata(a_subset2010))) + 
  geom_line(aes(x=date, y=value), color="blue", size=1.2, alpha=0.5) + xlab("") + ylab("")+
  geom_hline(yintercept = 0.01905980, color = "red") + theme_gdocs()


predic <- as.data.frame(predictions)
a<- xts(predic$predictions, order.by=index(model_data11))
a1 <- merge(a, model_data11$event)


### DASHBOARD PLOTS -------------
library('ggthemes')
ggplot(Trigger_s, aes(x = index(Trigger_s), y = Trigger_s)) +
  geom_line(color = "blue", size = 1.2,alpha = 0.5) +
  geom_hline(yintercept = -0.7, color = "red") +
  xlab("") + ylab("") +
  ggtitle("Trigger") +
  theme_gdocs()

end_date <- as.Date("2000-11-25")
half_year_before <- end_date - 180
sentiment_s_2010 <- window(sentiment_scale, end = end_date, start = half_year_before)
article_s_2010 <- window(article_smooth, end = end_date, start = half_year_before)

ggplot(sentiment_s_2010, aes(x = index(sentiment_s_2010), y = sentiment_s_2010)) +
  geom_line(color = "blue", size = 1.2,alpha = 0.5) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_gdocs()

ggplot(article_s_2010, aes(x = index(article_s_2010), y = article_s_2010)) +
  geom_line(color = "blue", size = 1.2,alpha = 0.5) +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_gdocs()


xts_proportions <- xts[,-1]
xts_proportions <- apply.weekly(xts_proportions, colMeans)
xts_proportions_2010<- window(xts_proportions, end = end_date, start = half_year_before)


p1 <- ggplot(xts_proportions_2010, aes(x = index(xts_proportions_2010), y = xts_proportions_2010$Promotional.activity)) +
  geom_line(color = "blue", size = 1,alpha = 0.5) +
  xlab("") + ylab("") +
  ggtitle("Promotional activity") +
  theme_gdocs()

p2 <- ggplot(xts_proportions_2010, aes(x = index(xts_proportions_2010), y = xts_proportions_2010$Product.launch)) +
  geom_line(color = "blue", size = 1,alpha = 0.5) +
  xlab("") + ylab("") +
  ggtitle("Product launch") +
  theme_gdocs()

p3 <- ggplot(xts_proportions_2010, aes(x = index(xts_proportions_2010), y = xts_proportions_2010$Product.recal)) +
  geom_line(color = "blue", size = 1,alpha = .5) +
  xlab("") + ylab("") +
  ggtitle("Product recal") +
  theme_gdocs()
p4 <- ggplot(xts_proportions_2010, aes(x = index(xts_proportions_2010), y = xts_proportions_2010$Investment.analysis)) +
  geom_line(color = "red", size = 1.3,alpha = 0.9) +
  xlab("") + ylab("") +
  ggtitle("Investment analysis") +
  theme_gdocs()

p5 <- ggplot(xts_proportions_2010, aes(x = index(xts_proportions_2010), y = xts_proportions_2010$Product.claims)) +
  geom_line(color = "blue", size = 1,alpha = 0.5) +
  xlab("") + ylab("") +
  ggtitle("Product claims") +
  theme_gdocs()


library(gridExtra)

# Assume you have five ggplot plots named plot1, plot2, plot3, plot4, plot5
plots <- list(p1, p2, p3, p4, p5)

grid.arrange(grobs = plots, ncol = 1, nrow = 5)








########## ROC CURVE (multiple models) ---------

plot(roc_curve2)
plot(roc_curve3, col = "blue", add = TRUE)
abline(a = 0, b = 1, col = "grey", lty = 2)
legend("bottomright", legend = c("Sentiment score", " Smoothed triger"), col = c("black", "blue"), lty = 1, lwd = 1)


plot(roc_curve4)
plot(roc_curve5, col = "blue", add = TRUE)
abline(a = 0, b = 1, col = "grey", lty = 2)
legend("bottomright", legend = c("Smoothed Sentiment", "Detrended articles"), col = c("black", "blue"), lty = 1, lwd = 1)


plot(roc_curve7)
plot(roc_curve8, col = "blue", add = TRUE)
abline(a = 0, b = 1, col = "grey", lty = 2)
legend("bottomright", legend = c("Detrended articles & sentiment", "Detrended articles & smoothed S"), col = c("black", "blue"), lty = 1, lwd = 1)

plot(roc_curve9)
plot(roc_curve10, col = "blue", add = TRUE)
abline(a = 0, b = 1, col = "grey", lty = 2)
legend("bottomright", legend = c("Smoothed articles & sentiment", "Smoothed articles & smoothed S"), col = c("black", "blue"), lty = 1, lwd = 1)


