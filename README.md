# Introduction
As part of John's Hopkins University Data Science specialization Capstone project in association with Swiftkey we need to build next word predictor Shiny App. Swiftkey has provided the text corpora for this project, which can be downloaded from the following location: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

The prediction model was built by applying techniques of Natural Language Processing(NLP) and using the skills developed during the specialization. Specifically, a 4-gram language model was developed, the final language model was pruned and optimized to about less than 2.4 MB in size.

The entire project development steps were performed under constraint of 3.86 GB available RAM and i3 CPU M350 2.27 GHz processor Win7 Laptop.

The final Shiny App developed can be accessed at: https://saneshashank.shinyapps.io/TextPredictorApp/

# Approach

The following approach was taken to build the final product:

1. Random Sampling of data was done starting with 5% initial data to do the exploratory data analysis. The milestone report markdown can be viewed in MilestoneReport.Rmd file.
2. Data Cleaning to remove numbers, punctuation, profanity filtering and remove foreign words .
3. A 4-3-2-1 gram data model was created from the sampled data.
4. Initial prototype model was built applying **Katz Backoff algorithm** on 3 gram data.
5. Discount factor for trigram and discount factor for bigram was calculated for highest accuracy keeping the data same using Cross validation. The final discount factors came out to be 0.25 for trigram and 1.5 for bigram.
6. Additionally the model was combined with 4 gram data model using **Stupid Backoff** to improve accuracy.
7. The sampled data was gradually increased to about 17% of the total corpus to optimize performance and accuracy using cross validation and benchmarking.
8. A benchmark accuracy of 19.33% was obtained using benchmark code at: https://github.com/hfoffani/dsci-benchmark
9. The benchmark code with benchmark wrapper function has also been included in repository.
10. Once data model was finalized fast look-up table was created for top 3 predictions.
11. finally the functionality was built in and deployed as Shiny App.

Additionally the development has been done keeping in mind to minimize complexity and optimize performance. There was also an additional constraint on available RAM (3.86 GB), for this purpose the following was done:

* **Quanteda** package was used for fast and efficient text tokenization.
* **Data.table** package was used for fast search and lookup.
* **filehash** package was used to store intermediate results and the principle of bring in memory only when required was followed.
* **stringr** package for fast and efficient string manipulations.
* all the functionality used has been modularized for easy implementation of data processing pipeline.

Some of the highlights of the App developed are as follows:

* Extremely low memory footprint of less than 2.5 MB, can be easily run on mobile platform.
* Very fast response time of 10.21 milli seconds.
* 3 word completion suggestions as soon a user starts typing in the word.
* 3 next word suggestions as soon as user hits space key after typing the word.


# Data Processing Pipeline

The following is the data processing pipeline (from left to right):

**Raw text Corpus** >> **Random Sampling** >> **Data Cleaning** >> **Data Model Creation** >> **Data Model Preprocessing** >> **Prediction Algorithm** >> **Parameter Tuning** >> **Benchmarking** >> **Create fast lookup with final predictor** >> **Finally wrap functionality in Shiny App.**

# Instruction for code usage

The code has been organized as below roughly modeled along the data processing pipeline. The code can be run and accessed in the order listed below:

* UtilFunctions.R: this file contains function for random sampling, tokenization and other data processing.
* NGramPreProcessing.R: this file contains the code for ngram preprocessing once ngrams have been created.
* DataModel.R: has the function data flow for data model creation (using functions present in above two files).
* Predictor.R: contains the code for prediction algorithm for bigram, trigram and tetragram prediction.
* BenchmarkRun.R: Contains the code for benchmark run along with benchmark wrapper function.
* CreateFastLookup.R: Contains code for creation of fast lookup tables for top 3 predictions for Shiny App.
* TextPredictorApp: this folder contains code for Shiny App.

The repository also contains the following additional files:

* MilestoneReport.Rmd: R markdown for milestone report.
* TextPredictionProductSlides.md: markdown for capstone presentation.

