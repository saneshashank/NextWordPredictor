setkey(bigram,first_word)
setkey(trigram,first_second)
setkey(tetragram,first_second_third)

#' This function is the bigram predictor, takes input as single word and predicts
#' next word based on bigram model.
#'  
#' @param FirstWord: input word
#' @param n: number of words to be filtered from unigram
#' @return : returns data.table containing predicted words with their probability.
BiGramNextWordPredictor <- function(FirstWord,n)
{
  setkey(unigram,Word)
  setkey(bigram,first_word)
  
  # find the leftover probability from bigram
  alpha_bigram <- bigram[.(FirstWord),.SD[1]]$LeftOver_Probability
  
  # Proceed if there is match in bigram
  if(!is.na(alpha_bigram))
  {
    # find the list of predicted words for matching bigrams
    SecondWordList <- bigram[.(FirstWord),]
    
    # set by descending order of frequency
    setorder(SecondWordList,-Frequency)
    
    # add logic to decide number passed
    if(nrow(SecondWordList) > n)
    {
      SecondWordList <- SecondWordList[,.SD[1:n]]
    }
    
    
    # extract the probabilities also
    bigram_probability_list <- SecondWordList$Discounted_Probability
    
    # set remaining prob to 1 initially
    remaining_probability <- 1
    
    # find the remaining probability in unigram except the words which are 
    # present in bigram
    remaining_probability <- remaining_probability - sum(unigram[J(SecondWordList$second_word)]$probability,na.rm = TRUE)
    
    # find adjusted probability of words in unigram
    unigram[,adjusted_prob:= (alpha_bigram*probability)/remaining_probability]
    
    #print(alpha_bigram)
    
    #print(remaining_probability)
    
    
    # for scondword list keep the adjusted probability same as original probability
    unigram[J(SecondWordList),adjusted_prob:=probability]
    
    setorder(unigram,-Frequency)
    
    # extract the top 5 unigram words predicted
    bi_uni_predicted_words <- data.table(Words= unigram[adjusted_prob!=probability,.SD[1:5]]$Word,Probability=unigram[adjusted_prob!=probability,.SD[1:5]]$adjusted_prob)
    
    # scale unigram predicted words
    #bi_uni_predicted_words[,Probability:=0.1*Probability]
    
    # merge with top n bigram words predicted
    bi_uni_predicted_words <- rbindlist(list(bi_uni_predicted_words,data.table(Words=SecondWordList$second_word,Probability=bigram_probability_list)))
    
    # set in descending order of probability
    setorder(bi_uni_predicted_words,-Probability)
    
    return(bi_uni_predicted_words)
  }
  else # as there is no match in bigram, just return the top n unigrams
  {
    setorder(unigram,-Frequency)
    
    bi_uni_predicted_words <- data.table(Words= unigram[,.SD[1:n]]$Word,Probability=unigram[,.SD[1:n]]$probability)
    
    # set in descending order of probability
    setorder(bi_uni_predicted_words,-Probability)
    
    return(bi_uni_predicted_words)
    
  }
}
#####################################################################################################

#' This function is the trigram predictor, takes input as last two words ("_" separated) and predicts
#' next word based on trigram model.
#'  
#' @param FirstSecondWord: two words ("_" separated) 
#' @param n: number of words to be filtered from unigram
#' @return : returns data.table containing predicted words with their probability.
TriGramNextWordPredictor <- function(FirstSecondWord,n)
{
  # seperate out the second word for usage in bigram
  SecondWord <- str_split(FirstSecondWord,pattern="_")[[1]][2]
  
  # find the leftover probability from trigram
  alpha_trigram <- trigram[.(FirstSecondWord),.SD[1]]$LeftOver_Probability
  
  if(!is.na(alpha_trigram))
  {
    # extracting third word list
    ThirdWordList <- trigram[.(FirstSecondWord),]$third_word
    
    # collect predicted words from trigram
    predicted_words <- data.table(Words=ThirdWordList,Probability=trigram[.(FirstSecondWord),]$Discounted_Probability)
    
    # call function to collect bigram and unigram predicted words
    temp <- BiGramNextWordPredictor(SecondWord,n)
    
    # scale by 0.1 to give higher weightage to trigram
    #temp[,Probability:=0.1*Probability]
    
    setkey(temp,Words)
    
    # remove words which were present in trigram
    temp <- temp[!(Words %in% ThirdWordList),]
    
    sum_of_adjusted_probs <- sum(temp$Probability)
    
    # calculate adjusted probabilities
    temp[,Probability:= (alpha_trigram*Probability)/sum_of_adjusted_probs]
    
    # Prepare the final prediction list
    predicted_words <- rbindlist(list(predicted_words,temp))
    
    # remove temp from memory
    #rm(temp)
    
    setorder(predicted_words,-Probability)
    
    return(predicted_words[,.SD[1:5]])
    #return(predicted_words)
  }
  else # check match in bigram
  {
    predicted_words <- BiGramNextWordPredictor(SecondWord,n)
    
    setorder(predicted_words,-Probability)
    
    return(predicted_words[,.SD[1:5]])
    #return(predicted_words)
    
  } 
  
}


#####################################################################################################

#' This function is the tetragram predictor, takes input as last three words ("_" separated) and predicts
#' next word based on tetragram model.
#'  
#' @param FirstSecondThirdWord: three words ("_" separated) 
#' @param n: number of words to be filtered from unigram
#' @return : returns data.table containing predicted words with their probability.
TetraGramNextWordPredictor <- function(FirstSecondThirdWord,n)
{
  # construct the second_third word to be passed to trigrampredictor
  second_third <- paste0( str_split(FirstSecondThirdWord,pattern="_")[[1]][2],"_",str_split(FirstSecondThirdWord,pattern="_")[[1]][3])
  
  if(!is.na(tetragram[.(FirstSecondThirdWord),.SD[1]]$Probability))
  {
    # extracting third word list
    FourthWordList <- tetragram[.(FirstSecondThirdWord),]$fourth_word
    
    # collect predicted words from trigram
    predicted_words <- data.table(Words=FourthWordList,Probability= tetragram[.(FirstSecondThirdWord),]$Probability)
    
    # call function to collect bigram and unigram predicted words
    temp <- TriGramNextWordPredictor(second_third,n)
    
    # scale by 0.1 to give higher weightage to tetragram
    temp[,Probability:=0.1*Probability]
    
    setkey(temp,Words)
    
    # remove words which were present in tetragram
    temp <- temp[!(Words %in% FourthWordList),]
    
    # Prepare the final prediction list
    predicted_words <- rbindlist(list(predicted_words,temp))
    
    # remove temp from memory
    #rm(temp)
    
    setorder(predicted_words,-Probability)
    
    return(predicted_words[,.SD[1:5]])
    #return(predicted_words)
    
  }
  else
  {
    TriGramNextWordPredictor(second_third,n)
    
  }
  
}

###################################################END############################################










