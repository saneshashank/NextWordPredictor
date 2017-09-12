#' SampleText function to do random sampling from provided text
#' connection provides the connection object to the text file
#' samplePercent indicates the percentage sampled (example: pass 0.25 to sample 25% data)
#' 
#' @param filePath: path of the text file to be sampled.
#' @param samplePercent: percentage of the text data to be sampled.
#' @param TextLength: length of the text file.
#' @return text_corpus_sample returns the random sampled text corpus. 
sampleText <- function(filePath,samplePercent,TextLength)
{
  
  # create file connection
  con <- file(filePath,"rb")
  
  text_sample_raw <- readLines(con,1,encoding = "UTF-8", skipNul = TRUE)
  j=2
  for(i in 2:TextLength)
  {
    if(rbinom(1,1,samplePercent))
    {
      
      text_sample_raw[j] <- readLines(con,1,encoding = "UTF-8", skipNul = TRUE)
      j <- j+1
    }
    else
    {
      text_sample_not_used <- readLines(con,1,encoding = "UTF-8", skipNul = TRUE)
      #rm(text_sample_not_used)
    }
    
  }
  
  # close file connection
  close(con)
  
  # Convert to corpus for processing and the remove the original
  # text from memory
  text_corpus_sample <- corpus(text_sample_raw)
  rm(text_sample_raw)
  
  # return sample text extracted
  return(text_corpus_sample)
  
}

############################################################################################
###########################################################################################

#'This function takes as input token file containing word tokens
#'and returns a data.table containing Word and Frequency columns
#'
#' @param token: ngram token derived using quanteda package
#' @return FrequencyTable: data.table containing word frequencies having columns 'Word' and 'Frequency'. 
#' @export
WordFrequencyTable <- function(token)
{
  # create document frequency matrix from token
  ## create frequency vector
  ### remove dfm from memory  
  dfm_ngram <- dfm(token)
  FrequencyTable <- docfreq(dfm_ngram)
  rm(dfm_ngram)
  
  # convert the numeric frequency vector to data table
  require(data.table)
  FrequencyTable <- data.table(Word =names(FrequencyTable),Frequency = FrequencyTable)
  
  return(FrequencyTable)
}

############################################################################################
###########################################################################################

#'This function takes two data tables as input an merges the two basedon 'Word' column
#'
#' @param table_1: table 1 to be generated using function WordFrequencyTable
#' @param table_2: table 2 to be generated using function WordFrequencyTable
#' @return table_1 merged data.table.
MergeFrequencyTables <- function(table_1,table_2)
{
  # bind the functions
  require(data.table)
  table_1 <- rbindlist(list(table_1,table_2))
  
  # group by based on word and then sum up the frequency
  require(dplyr)
  table_1 <- group_by(table_1,Word)%>%
             summarise(Frequency=sum(Frequency))
  
  # order by Frequency descending
  setorder(table_1,-Frequency)
  
  # return merged table
  return(table_1)
}  


############################################################################################
###########################################################################################

#' This function takes in sample text and creates unigram,bigram and trigram tokens.
#' The functio does not return anything rather saves the tokens created in filehash database
#' with the names provided.
#' 
#' @param SampleText: sample input text corpus(created using quanteda) to be tokenized.
#' @param FileName1Gram: filename for unigram token.
#' @param FileName2Gram: filename for bigram token.
#' @param FileName3Gram: filename for trigram token.
#' @param DbObject: filehash db object.
WordTokenizer <- function(SampleText,FileName1Gram,FileName2Gram,FileName3Gram,FileName4Gram,DbObject)
{
  # tokenize corpus first based on sentences
  token1 <- tokenize(SampleText,what="sentence",remove_url = TRUE,remove_numbers =   TRUE, remove_punct = TRUE,remove_symbols = TRUE,remove_hyphens=TRUE,remove_twitter=TRUE, simplify=TRUE)
  
  # Convert to tokens,removing punctuation and symbols
  token1 <- tokens(token1,remove_punct = TRUE,remove_symbols = TRUE,remove_url = TRUE,remove_numbers = TRUE,remove_hyphens=TRUE,remove_twitter=TRUE)
  
  # url to download list of profanity words
  profanity_url <- "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
  
  # download as csv if file does not exist
  if (!file.exists("ProfaneWords.csv")){
    # download profane wordsas csv file
    download.file(profanity_url, "ProfaneWords.csv")
  }
  
  # get the vector list of Profane words
  ProfaneWords <-  read.csv("ProfaneWords.csv",header = FALSE,stringsAsFactors = FALSE)
  ProfaneWords <- unlist(ProfaneWords$V1)
  
  # remove profane words from the tokens created
  token1 <- removeFeatures(token1,ProfaneWords)
  
  # remove ProfaneWords from memory
  rm(ProfaneWords)
  
  # save the unigram token in db
  dbInsert(DbObject,FileName1Gram,token1)
  print("unigram created!!")
  
  token2 <- tokens_ngrams(token1,n=2)
  # save the bigram token in db
  dbInsert(DbObject,FileName2Gram,token2)
  # remove trigram from token
  rm(token2)
  
  print("bigram created!!")
  
  token3 <- tokens_ngrams(token1,n=3)
  # save the trigram token in db
  dbInsert(DbObject,FileName3Gram,token3)
  # remove trigram from token
  rm(token3)
  
  print("trigram created!!")
  
  token4 <- tokens_ngrams(token1,n=4)
  # save the trigram token in db
  dbInsert(DbObject,FileName4Gram,token4)
  # remove trigram from token
  rm(token4)
  
  # remove token1 as well
  rm(token1)
}


####################################################################################
###################################################################################
#' This function calculates discounted frequency for bigram or trigram based
#' on input discount factor. Can be used during dicount factor tuning phase.
#' 
#' @param ngram: bigram or trigram data.table.
#' @param DiscountFactor: discount factor.
#' @param isBigram: TRUE if bigram FALSE if trigram
CalculateDiscountedFrequency <- function(ngram,DiscountFactor,isBigram)
{
    # set discount factor
    d <- DiscountFactor
  
    if(isBigram)
    {
      # Calculate group frequencies,group count and discounted frequencies
      ngram <- ngram[,Group_Freq_Sum:=sum(Frequency),by=first_word]
      ngram <- ngram[,Group_Count:=.N,by=first_word]
    }
    else
    {
      # Calculate group frequencies,group count and discounted frequencies
      ngram <- ngram[,Group_Freq_Sum:=sum(Frequency),by=first_second]
      ngram <- ngram[,Group_Count:=.N,by=first_second]
    }
  
    ngram <- ngram[,Discounted_Frequency:=abs(Frequency-d)]
    ngram <- ngram[,LeftOver_Probability:=(d*Group_Count)/Group_Freq_Sum]
    ngram <- ngram[,Discounted_Probability:= Discounted_Frequency/Group_Freq_Sum] 
  
}
