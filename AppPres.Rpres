Coursera Data Science Capstone Presentation
========================================================
author: Claudia Morales
date: March 15, 2017
autosize: true

The Final Product: Word Prediction App
========================================================

- The app created can be tested here: <a href = "https://claud1603.shinyapps.io/PredictApp/"> My App</a>
- Codes and documentation are found in <a href = "https://github.com/ClaudiaMorales/FinalProjectDataScience/"> my GitHub repository</a> 
- The following slides describe the application creation process and usage


Data Cleaning & Processing
========================================================

The data used in this app can be downloaded <a href = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"> here.</a>  For the purpose of this exercise, only the English versions were used.

- The raw text documents were transformed into a format more suitable for automated manipulation.  All was 
converted to lower case, stripped of whitespace and stopwords as well as profanity were removed.

- Once the data is cleaned, a term-document matrix (TDM), a matrix of words or phrases and their frequencies was created.

- This <a href = "https://rpubs.com/claud1603/250272"> Milestone Report</a> contains a more detailed summary of the process above.


Description of the Algorithm
========================================================

The algorithm used is a simplified <a href = "https://en.wikipedia.org/wiki/Katz%27s_back-off_model"> Katz's back-off model.</a>  

My actual model can be found in the GitHub link in the first slide.

This model, backs off to smaller n-grams when a key is not found in the larger n-gram. The maximum n-gram handled in this project is a trigram.

The word returned is the match found in the largest n-gram where the key is actually found.



How the App Works and Usage
========================================================


 In summary, this application is desigend to take a key word or phrase, match it to the most frequent "n-1" term found in the TDM
 and return the "nth" word with the highest frequency.
 
 The possible usage for this application is limitless. For example, this app could be incorporated in the user interface of your 
 own company's website!! 
 
 The user will simply type in a word in the search bar, and the algorithm would auto-fill the most likely next word, much like 
 the google search bar! 
 
 This in turn, will allow you to study consumer behavior of most frequently searched products or trends within your company and line
 of business.    



