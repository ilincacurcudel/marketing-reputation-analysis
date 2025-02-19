## Project Summary

Goal: Develop a model to detect reputation-impacting events for Kellogg’s using media articles.

Approach: Use text mining, sentiment analysis, and machine learning models to quantify media sentiment and link it to potential reputation events.

### How to achieve this goal?

1. Data Collection & Preprocessing:
- Loaded news articles mentioning Kellogg’s.
- Cleaned and tokenized text to extract key themes using LDA topic modelling.
- Filtered articles by LexisNexis relevance scores to focus on significant publications.

2. Sentiment & Topic Analysis:
- Computed sentiment scores for each article.
- Classified articles into key themes. 
- Examined historical sentiment trends.

3. Reputation Event Detection:
- Aggregated sentiment scores weekly and applied detrending & smoothing techniques.
- Created a Trigger Score by combining sentiment shifts and article volume trends.
- Matched trigger score spikes to real-world reputation events (e.g., recalls, lawsuits, PR crises).

4. Model Development & Evaluation:
- Trained logistic regression models using the event data and sentiment scores.
- Tested multiple models (e.g., raw sentiment, smoothed sentiment, detrended article trends).
- Evaluated models using ROC curves, confusion matrices, and Youden’s index.

### Results & recommendations

#### Key Findings:
- The Trigger Score Model effectively identified major events.
- Smoothed sentiment and article volume data provided better predictive accuracy.
- Model 11, which combined sentiment, article trends, and LDA topics, had the highest accuracy in predicting reputation-impacting events.

#### Future work: 
- Expand the model to analyze multiple companies across industries.
- Enhance model interpretability using SHAP values.
- Explore deep learning approaches for sentiment and event classification.

# Methodology: a more in-depth analysis 

## 1. **Selecting relevant articles**
- The dataset consists of 8,501 articles.
- [LexisNexis Relevancy Scores](https://supportcenter.lexisnexis.com/app/answers/answer_view/a_id/1098162/~/smartindexing) (ranging from 85%-99%) were used to filter articles.
- Articles with less than 200 words and newswire sources (brief press releases) were removed.

   ![Screenshot 2025-02-19 at 14 58 36](https://github.com/user-attachments/assets/50a1909b-946c-4362-9753-9aa3a9d2213d)
  
## 2. **Preparing Data for LDA Topic Modeling**

The next step was to prepare the relevant articles for estimating a Latent Dirichlet Allocation (LDA) model. To estimate an LDA model, it is important to create a Document-Feature Matrix (DFM) that contains only relevant information. The following steps were taken to create a relevant DFM:
- Tokenization & Cleaning: Removed punctuation, symbols, numbers, URLs, and uppercases.
- Bi-grams Identification: Important bi-grams (phrases that hold distinct meanings) were selected
per share (relevant because it differs from share/sharing in other context)
  - kellogg company (might be relevant as news outlets that mention company might be more focused on business)
  - kellogg co (Same as previous entry)
  - market data (This indicates market news)
  - nyse k (This is a reference to the stock ticker that would otherwise be lost)
  - special k (This is an important brand of Kelogg’s)
  - rice krispies (This is an important product of Kelogg’s)
  - Battle creek (This is the town were Kelogg’s is headquartered)
- **Stemming**: is the process of reducing a word to its base form, or stem.
For example: the stem of ”eating,” ”eats,” and ”eaten” is ”eat.”
This process merges words with similar meanings into single tokens and reduces dimensionality.
![Screenshot 2025-02-19 at 15 06 37](https://github.com/user-attachments/assets/30022568-22eb-465a-956e-3ec217b89d2c)

There were a significant number of words that added little meaning, known as ”stop words,” which can be removed using pre-constructed dictionaries. Therefore, all words that appeared less than 50 times,
all words that had less than 3 characters, and all words that appeared in more than 50% of the text were removed. See the last result in the figure below. 

![Rplot1](https://github.com/user-attachments/assets/b781a492-d1df-4057-a377-1f114f5f9839)


## 3. **Estimating the LDA**

- LDA (Latent Dirichlet Allocation) was applied to uncover hidden topics.
- Initially, 10 topics were extracted, then grouped into 5 key themes:

![Screenshot 2025-02-19 at 15 15 22](https://github.com/user-attachments/assets/8e04798d-b954-4815-91d2-35795bf20508)

![Rplot2](https://github.com/user-attachments/assets/23239d15-50ec-4862-bee8-6b8105173635)


## 4. **Sentiment analysis:**

- Lexicon-based sentiment analysis was applied with valence shifters (to adjust for intensifiers and negations).
- Sentiment Score Calculation:
  - Positive Sentiment → Score near 1
  - Neutral Sentiment → Score near 0
  - Negative Sentiment → Score near -1
![Screenshot 2025-02-19 at 15 19 12](https://github.com/user-attachments/assets/5253fa4c-93e8-445d-a418-26f6571f38fc)

# Results & Reputation Event Detection

If there is the likelihood for one single negative article to make reference to Kellogg’s, this does not necessarily imply that there is likely a negative event happening for the company. _Combining the smoothed-out amount of articles and the sentiment scores together_ leaves us with a normalized vector, giving more weight to when there are more negative articles written in a certain time frame. A threshold of **-0.6** was chosen and found to be the most optimal given the events previously discussed and given data. The first figure below displays the _Smoothed Amount of Articles_, while the second figure highlights the _Smoothed Sentiment Score_. 

![Screenshot 2025-02-19 at 15 26 34](https://github.com/user-attachments/assets/c0182005-69e0-4331-8d4e-8be72c843940)

![Screenshot 2025-02-19 at 15 26 38](https://github.com/user-attachments/assets/ec6e0050-6562-4bea-8960-037f1d5c0685)

In 2015, Kellogg’s reputation was highly positive following its recognition as "America’s most reputable consumer company" due to sustainability efforts. This event explains the increase in the number of articles between 2014-2015, and hence the associated positive sentiment score about the reputation of the company.

The above-described feature was plotted: 
![Screenshot 2025-02-19 at 15 30 33](https://github.com/user-attachments/assets/b91077ef-660f-468e-82bd-1ad663360ba5)

The other associated triggered reputation events (6) that are noticeable in the graph above were highlighted by the following news events:
  - November 2000: glass found in cereal box, lobbying against keeping sugar amount
from box ingredients list
  - June 2005: Kellogg’s Tony the Tiger voice actor died
  - April 2009: FTC’s complaint about deceptive claims
  - June 2011: Kelloggs want to ’self-regulate’ junk food advertising to children
  - December 2012: 52-week low for the stock
  - June 2014: Many job layoffs at the headquarters

# R packages used

1.Data Manipulation & Processing
- data.table → Efficient table manipulation for large datasets
- dplyr → Data wrangling (filtering, summarizing, mutating)
  
2. Text Processing & NLP
- [quanteda](https://quanteda.io/) → Tokenization, n-grams, text preprocessing
- quanteda.textplots → Visualizing text data
- quanteda.textstats → Computing text statistics (TF-IDF, keyness, etc.)
- [spacyr](https://cran.r-project.org/web/packages/spacyr/vignettes/using_spacyr.html) → Part-of-Speech (POS) tagging and named entity recognition

3. Topic Modeling & Sentiment Analysis
- [seededlda](https://github.com/koheiw/seededlda) → LDA Topic Modeling with seed words
- [sentometrics](https://sentometrics-research.com/sentometrics/) → Sentiment analysis with lexicons & machine learning
  
4. Time Series & Financial Analysis
- xts → Handling time-series data

5. Statistical Modeling & Hypothesis Testing
- lmtest → Linear model diagnostics and hypothesis testing
- ROCR → Performance evaluation for classification models

6. Data Visualization
- ggplot2 → General-purpose plotting
- [ggwordcloud](https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html) → Creating word clouds
- gridExtra → Combining multiple plots
- cowplot → Enhancing ggplot visualizations

