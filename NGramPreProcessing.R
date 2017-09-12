#' This function takes in raw Ngram (created by WordTokenizer function in UtilFunctions.R)
#' and cleans the ngram to remove garbage values,also creates additional columns required
#' in Predictor.R functions. Additionally calculates the discounted frequencies for bigram and trigram.
#' 
#' @param ngram: ngram data.table
#' @param N: which type of ngram possible values 1,2,3 and 4
#' @param DiscountFactor: discount factor for frequency discounting
#' @param ThresholdFrequency: ngrams below ThresholdFrequency will be removed.
#' @return : preprocessed ngram data.table
NgramPreProcessing <-  function(ngram,N,DiscountFactor,ThresholdFrequency)
{
  # need to remove such entries
  # _o_so,_____lil,____dictator,______________i'm
  # #1_i,#10beautifulpeopleifollow_most,#14_will (any hashtags)
  ngram <- ngram[!grepl('^[_+|#]',Word)]
  ngram <- ngram[!grepl('^[@]',Word)]
  ngram <- ngram[!grepl('[0-9]',Word)]
  ngram <- ngram[!grepl(':',Word)]
  
  # remove any n gram starting with numbers.
  ngram <- ngram[!grepl('^[0-9]',Word)]
  
  #' also think of removing the following examples:
  #' at___ , because______ ,is_______
  #' also remove single words except a,I etc.
  #' also remove any url's starting from www.
  ngram <- ngram[!grepl('[a-z]__+',Word)]
  ngram <- ngram[!grepl("www(.+)",Word)]
  ngram <- ngram[!grepl("^[b-hj-z]$",Word)]
  
  # convert all words to ASCII and replace all non-convertibles to _NA and finally remove them.
  ngram$Word <- iconv(ngram$Word , from = "UTF-8", to = "ASCII", sub = "_NA")
  ngram <- ngram[!grepl("_NA",Word)]
  
  # remove any ngrams below threshold frequency
  ngram <- ngram[Frequency > ThresholdFrequency]
  
  # convert to ascii (remove any unicode characters)
  require(stringi)
  ngram <- ngram[,Word:=stri_trans_general(Word, "latin-ascii")]
  
  if(N==1)
  {
    # remove any words containing _ or -
    ngram <- ngram[!grepl('_|-',Word)]
    
    # unigram[grepl("[^[:alnum:][:space:]\'<>]",Word)] -- check this and then decide many words in this list are useful.
   
  }
  
  # if it is a bigram do additional bigram processing
  if(N==2)
  {
    # replace words starting from zz
    ngram <- ngram[!grepl('^[zz+]',Word)]
    
    # create new columns containing first and second words
    require(stringr)
    ngram <- ngram[,first_word := sapply(Word,function(X){str_split(X,pattern="_")[[1]][1]})]
    ngram <- ngram[,second_word := sapply(Word,function(X){str_split(X,pattern="_")[[1]][2]})]
    
    # remove all unicode
    ngram <- ngram[,first_word:=stri_trans_general(first_word, "latin-ascii")]
    ngram <- ngram[,second_word:=stri_trans_general(second_word, "latin-ascii")]
    
    # do some additional cleaning on bigrams
    ngram <- ngram[!grepl('^[#]',second_word)]
    ngram <- ngram[!grepl("a's",first_word)]
    ngram <- ngram[!grepl("-",first_word)]
    ngram <- ngram[!grepl("-|#",Word)]
    ngram <- ngram[!grepl('\\.',Word)]
    
  }
  
  # if it is a trigram do additional trigram processing
  if(N==3)
  {
    # create new columns containing first and second words
    require(stringr)
    ngram <- ngram[,first_word := sapply(Word,function(X){str_split(X,pattern="_")[[1]][1]})]
    ngram <- ngram[,second_word := sapply(Word,function(X){str_split(X,pattern="_")[[1]][2]})]
    ngram <- ngram[,third_word := sapply(Word,function(X){str_split(X,pattern="_")[[1]][3]})]
    ngram <- ngram[,first_second:= paste0(first_word,'_',second_word)]
    ngram <- ngram[,second_third:= paste0(second_word,'_',third_word)]
    
    #' remove any repeating stop words like:
    #' a_a_and ,a_a_person,and_and_i,and_and_cons etc.
    ngram[first_word==second_word & first_word %in% stopwords(kind="english") & first_word != "very"]
    
   
    # some additional cleaning up for trigrams.
    ngram <- ngram[!grepl("a's",first_word)]
    ngram <- ngram[!grepl('^[Follo|#]',first_word)]
    ngram <- ngram[!grepl("-|#",first_word)]
    ngram <- ngram[!grepl("-|#",Word)]
    ngram <- ngram[!grepl('\\.',Word)]
  }
  
  if(N==4)
  {
    ngram <- ngram[!grepl("-|#",Word)]
    ngram <- ngram[!grepl('\\.',Word)]
    
    require(stringr)
    ngram <- ngram[,first_second_third := sapply(Word,function(X){paste0(str_split(X,pattern="_")[[1]][1],"_",str_split(X,pattern="_")[[1]][2],"_",str_split(X,pattern="_")[[1]][3])})]
    ngram <- ngram[,fourth_word := sapply(Word,function(X){str_split(X,pattern="_")[[1]][4]})]
    #ngram <- ngram[,second_third := sapply(Word,function(X){past0(str_split(X,pattern="_")[[1]][2],"_",str_split(X,pattern="_")[[1]][3])})]
    
  }
  
  # Calculate probabilties:
  if(N==1)
  {
    # find the sum of unigram frequencies
    unigram_freq_sum <- sum(ngram$Frequency)
    # calculate word probabilities
    ngram <- ngram[,probability:= Frequency/unigram_freq_sum]
  }
  else if(N==2|N==3)
  {
    # set discount factor
    d <- DiscountFactor
    
      if(N==2)
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
  else
  {
    # Calculate group frequencies,group count and discounted frequencies
    ngram <- ngram[,Group_Freq_Sum:=sum(Frequency),by=first_second_third]
    ngram <- ngram[,Group_Count:=.N,by=first_second_third]
    ngram <- ngram[,Probability:= Group_Count/Group_Freq_Sum] 
    
  }
 
  # order by descending order of frequency
  setorder(ngram,-Frequency)
  
  # set key for fast searching
  if(N==1)
  {
    setkey(ngram,Word)
  }
  if(N==2)
  {
    setkey(ngram,first_word)
  }
  if(N==3)
  {
    setkey(ngram,first_second)
  }

  # return pre-processed ngram
  return(ngram)
}
