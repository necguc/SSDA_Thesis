# -*- coding: utf-8 -*-
"""Sentiment_Analysis_Thesis

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/16p0i86SVEE9mK2NoOfpZ-GUaK5X2Wfn1
"""

import pandas as pd
from transformers import AutoTokenizer, AutoModelForSequenceClassification, pipeline
import torch
from google.colab import drive
import numpy as np

# Load the dataset
file_path = "filtered_speeches_with_highest_frame.xlsx"
df = pd.read_excel(file_path)

# Load the tokenizer and model
tokenizer = AutoTokenizer.from_pretrained("savasy/bert-base-turkish-sentiment-cased")
model = AutoModelForSequenceClassification.from_pretrained("savasy/bert-base-turkish-sentiment-cased")

# Check if GPU is available
device = 0 if torch.cuda.is_available() else -1

# Load the sentiment analysis pipeline
sentiment_pipeline = pipeline("sentiment-analysis", model=model, tokenizer=tokenizer, device=device)

# Function to make labels human-readable
def map_label(label):
    label_mapping = {
        'LABEL_0': 'negative',
        'LABEL_1': 'positive'
    }
    return label_mapping.get(label, label)

# Split the dataset into chunks
chunk_size = 5000  # Number of rows per chunk
chunks = [df[i:i + chunk_size] for i in range(0, df.shape[0], chunk_size)]

# Process each chunk and save the results
results = []
for i, chunk in enumerate(chunks):
    sentiment_labels = []
    sentiment_scores = []

    for paragraph in chunk['Cleaned_Paragraph']:
        result = sentiment_pipeline(paragraph, truncation=True, padding=True)
        label, score = result[0]['label'], result[0]['score']
        sentiment_labels.append(map_label(label))
        sentiment_scores.append(score)

    chunk['sentiment'] = sentiment_labels
    chunk['sentiment_score'] = sentiment_scores
    results.append(chunk)

    print(f"Processed chunk {i + 1}/{len(chunks)}")

# Combine all chunks into a single DataFrame
final_df = pd.concat(results)

# Check the first few rows
print(final_df[['Cleaned_Paragraph', 'sentiment', 'sentiment_score']].head())

# Save the results
output_path = "speeches_with_sentiment_analysis_final2.xlsx"
final_df.to_excel(output_path, index=False)
print("Results saved to:", output_path)

# Statistical analysis and distribution
import matplotlib.pyplot as plt
import seaborn as sns

# Find the total number of rows
total_rows = final_df.shape[0]
print(f"Total Number of Rows: {total_rows}")

# Find the count of NEGATIVE and POSITIVE labels
label_counts = final_df['sentiment'].value_counts()
print("Sentiment Counts:\n", label_counts)

# Check that the labels are correctly mapped
print(final_df['sentiment'].unique())

# Visualize the distribution with a histogram
plt.figure(figsize=(10, 6))
sns.histplot(data=final_df, x='sentiment_score', hue='sentiment', multiple='stack', bins=20, palette='viridis')
plt.title('Sentiment Score Distribution')
plt.xlabel('Sentiment Score')
plt.ylabel('Count')
plt.legend(title='Sentiment')
plt.show()

# General statistical information
general_stats = final_df['sentiment_score'].describe()
print("General Sentiment Score Statistics:\n", general_stats)

# Separate statistical information for NEGATIVE and POSITIVE scores
if 'negative' in final_df['sentiment'].values:
    negative_stats = final_df[final_df['sentiment'] == 'negative']['sentiment_score'].describe()
    print("\nNEGATIVE Sentiment Score Statistics:\n", negative_stats)
else:
    print("\nNo NEGATIVE label found.")

if 'positive' in final_df['sentiment'].values:
    positive_stats = final_df[final_df['sentiment'] == 'positive']['sentiment_score'].describe()
    print("\nPOSITIVE Sentiment Score Statistics:\n", positive_stats)
else:
    print("\nNo POSITIVE label found.")