# load the required libraries
library(quanteda)
library(data.table)
library(LaF)
library(filehash)
library(stringi)
library(stringr)
library(dplyr)

# twitter file path
filePathTwitt <- "en_US\\en_US.twitter.txt"

# blog file path
filePathBlog <- "en_US\\en_US.blogs.txt"

# news file path
filePathNews <- "en_US\\en_US.news.txt"


# find the number of lines in twitter coprora
text_length_twitt <- determine_nlines(filePathTwitt)
# find the number of lines in Blog coprora
text_length_blog <- determine_nlines(filePathBlog)
# find the number of lines in News coprora
text_length_news <- determine_nlines(filePathNews)

# create textCorpus_DB database to store computed values
dbCreate("textCorpus_DB",type="RDS")

# initialize db object
db <- dbInit('textCorpus_DB',type="RDS")

# extract 20% of blog random sampled data
text_blog_20 <- sampleText(filePathBlog,0.20,text_length_blog)
text_blog_30 <- sampleText(filePathBlog,0.30,text_length_blog)

# call WordTokenizer util function to tokenize data into 1-4 grams and store in database
WordTokenizer(text_blog_20,"token1_20_blog","token2_20_blog","token3_20_blog","token4_20_blog",db)

WordTokenizer(text_blog_30,"token1_30_blog","token2_30_blog","token3_30_blog","token4_30_blog",db)
# remove text corpus when done
rm(text_blog_30)

# extract 15% of twitter random sampled data
text_twitt_15 <- sampleText(filePathTwitt,0.15,text_length_twitt)

text_twitt_20 <- sampleText(filePathTwitt,0.2,text_length_twitt)

WordTokenizer(text_twitt_15,"token1_15_twitt","token2_15_twitt","token3_15_twitt","token4_15_twitt",db)
rm(text_twitt_15)

WordTokenizer(text_twitt_20,"token1_20_twitt","token2_20_twitt","token3_20_twitt","token4_20_twitt",db)
rm(text_twitt_20)


text_news_10 <- sampleText(filePathNews,0.10,text_length_news)

WordTokenizer(text_news_10,"token1_10_news","token2_10_news","token3_10_news","token4_10_news",db)
rm(text_news_10)

#####################Creating final Unigram Model###############################

# fetch blog token from database and convert the token to frequency table
token1 <- filehash::dbFetch(db,"token1_20_blog")
unigram_blog <- WordFrequencyTable(token1)
rm(token1)

token1 <- filehash::dbFetch(db,"token1_30_blog")
unigram_blog <- WordFrequencyTable(token1)
rm(token1)



# order by frequency and remove any words having frequency 5 or less:
setorder(unigram_blog,-Frequency)
unigram_blog <- unigram_blog[Frequency>5]

# fetch twitter token from database and convert the token to frequency table
token1 <- filehash::dbFetch(db,"token1_15_twitt")
unigram_twitt <- WordFrequencyTable(token1)
rm(token1)

token1 <- filehash::dbFetch(db,"token1_20_twitt")
unigram_twitt <- WordFrequencyTable(token1)
rm(token1)

# order by frequency and remove any words having frequency 5 or less:
setorder(unigram_twitt,-Frequency)
unigram_twitt <- unigram_twitt[Frequency>5]

# fetch news token from database and convert the token to frequency table
token1 <- filehash::dbFetch(db,"token1_10_news")
unigram_news <- WordFrequencyTable(token1)
rm(token1)

# order by frequency and remove any words having frequency 5 or less:
setorder(unigram_news,-Frequency)
unigram_news <- unigram_twitt[Frequency>5]

# now we will merge the unigrams, however merging will be done so as to augment only low frequency unigrams
# we observe that almost 90% words have frequency less than 500 while corresponding probability is less than 1%
unigram<-MergeFrequencyTables(unigram_blog,unigram_twitt[Frequency<500])
unigram<-MergeFrequencyTables(unigram,unigram_news[Frequency<500])
rm(unigram_blog,unigram_twitt,unigram_news)

# convert back to data.table
unigram <- data.table(unigram)

#' Now do unigram pre-processing
#' this will remove bad word values and non ascii values and calculate probabilities
unigram <- NgramPreProcessing(unigram,1,0,0)

# save unigram in database for future use:
dbInsert(db,"unigram",unigram)

####################################################################################################################

#############################Creating final bigram model###########################################################

# fetch blog token from database and convert the token to frequency table
token2 <- filehash::dbFetch(db,"token2_30_blog")
bigram_blog <- WordFrequencyTable(token2)
rm(token2)

# order by frequency.
setorder(bigram_blog,-Frequency)

# fetch twitt token from database and convert the token to frequency table
token2 <- filehash::dbFetch(db,"token2_20_twitt")
bigram_twitt <- WordFrequencyTable(token2)
rm(token2)

setorder(bigram_twitt,-Frequency)

# fetch news token from database and convert the token to frequency table
token2 <- filehash::dbFetch(db,"token2_10_news")
bigram_news <- WordFrequencyTable(token2)
rm(token2)

setorder(bigram_news,-Frequency)

# merge bigrams to augment low frequency bigrams
bigram <-MergeFrequencyTables(bigram_blog,bigram_twitt[Frequency<50])
#bigram <-MergeFrequencyTables(bigram,bigram_news[Frequency<50])
rm(bigram_blog,bigram_twitt,bigram_news)

bigram  <- data.table(bigram)

bigram <-NgramPreProcessing(bigram,2,1.5,1)

# save bigram in database for future use:
dbInsert(db,"bigram",bigram)

####################################################################################################################

#############################Creating final trigram model###########################################################

token3 <- filehash::dbFetch(db,"token3_30_blog")
trigram_blog <- WordFrequencyTable(token3)
rm(token3)

setorder(trigram_blog,-Frequency)
trigram_blog <- trigram_blog[Frequency>1]


token3 <- filehash::dbFetch(db,"token3_20_twitt")
trigram_twitt <- WordFrequencyTable(token3)
rm(token3)

setorder(trigram_twitt,-Frequency)
trigram_twitt <- trigram_twitt[Frequency>1]

# token3 <- filehash::dbFetch(db,"token3_10_news")
# trigram_news <- WordFrequencyTable(token3)
# rm(token3)
# 
# setorder(trigram_news,-Frequency)
# trigram_news <- trigram_news[Frequency>1]

trigram <-MergeFrequencyTables(trigram_blog,trigram_twitt)
#trigram <-MergeFrequencyTables(trigram,trigram_news)
rm(trigram_blog,trigram_twitt,trigram_news)

trigram  <- data.table(trigram)
trigram <-NgramPreProcessing(trigram,3,0.25,1)

# save bigram in database for future use:
dbInsert(db,"trigram",trigram)

unigram <- dbFetch(db,"unigram")
bigram <- dbFetch(db,"bigram")
trigram <- dbFetch(db,"trigram")
tetragram <- dbFetch(db,"tetragram")

######################################################################################################################
#############################Creating final tetragram model###########################################################

token4 <- filehash::dbFetch(db,"token4_30_blog")
tetragram_blog <- WordFrequencyTable(token4)
rm(token4)

setorder(tetragram_blog,-Frequency)
tetragram_blog <- tetragram_blog[Frequency>1]

token4 <- filehash::dbFetch(db,"token4_20_twitt")
tetragram_twitt <- WordFrequencyTable(token4)
rm(token4)

setorder(tetragram_twitt,-Frequency)
tetragram_twitt <- tetragram_twitt[Frequency>1]

token4 <- filehash::dbFetch(db,"token4_10_news")
tetragram_news <- WordFrequencyTable(token4)
rm(token4)

setorder(tetragram_news,-Frequency)
tetragram_news <- tetragram_news[Frequency>1]

tetragram <-MergeFrequencyTables(tetragram_blog,tetragram_twitt)
#tetragram <-MergeFrequencyTables(tetragram,tetragram_news)
rm(tetragram_blog,tetragram_twitt,tetragram_news)

dbInsert(db,"tetragram",tetragram)
tetragram <- dbFetch(db,"tetragram1")
tetragram  <- data.table(tetragram)
tetragram <-NgramPreProcessing(tetragram,4,0,0)