## Project Summary

Goal: Develop a model to detect reputation-impacting events for Kellogg’s using media articles.

Approach: Use text mining, sentiment analysis, and machine learning models to quantify media sentiment and link it to potential reputation events.

Key Techniques:
1. Latent Dirichlet Allocation (LDA) to classify articles into topics.
2. Sentiment Analysis using lexicons to quantify media tone.
3. Time Series Analysis to track sentiment changes and article trends over time.
4. Logistic Regression Models to detect patterns that signal major reputation events.


## How to achieve this goal?

1. Data Collection & Preprocessing:
- Loaded news articles mentioning Kellogg’s.
- Cleaned and tokenized text to extract key themes using LDA topic modeling.
- Filtered articles by LexisNexis relevance scores to focus on significant publications.

2. Sentiment & Topic Analysis:
- Computed sentiment scores for each article.
- Classified articles into key themes: Promotional Activity, Product Launch, Product Recalls, Investment Analysis, and Product Claims.
- Examined historical sentiment trends.

3. Reputation Event Detection:
- Aggregated sentiment scores weekly and applied detrending & smoothing techniques.
- Created a Trigger Score by combining sentiment shifts and article volume trends.
- Matched trigger score spikes to real-world reputation events (e.g., recalls, lawsuits, PR crises).

4. Model Development & Evaluation:
- Trained logistic regression models using the event data and sentiment scores.
- Tested multiple models (e.g., raw sentiment, smoothed sentiment, detrended article trends).
- Evaluated models using ROC curves, confusion matrices, and Youden’s index.

## Results & recommendations

### Key Findings:
- The Trigger Score Model effectively identified major events.
- Smoothed sentiment and article volume data provided better predictive accuracy.
- Model 11, which combined sentiment, article trends, and LDA topics, had the highest accuracy in predicting reputation-impacting events.

### Future work: 
- Expand the model to analyze multiple companies across industries.
- Enhance model interpretability using SHAP values.
- Explore deep learning approaches for sentiment and event classification.

# Methodology: From Data to Reputation Events
