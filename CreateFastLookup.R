
#' This function takes in list of words (ngram words) and ngram number (2,3 or 4) and
#' create prediction lookup table based on top 3 response of the Predictor.R functions.
#' 
#' @param WordList: ngram word list.
#' @param NgramNumber: type of ngram word list, possible values are 2,3 or 4.
#' @return : Lookup table with columns Input (for Input word/ngram), Pred1,Pred2,Pred3(for top 3 predictions)
#' and ProbabilitySum for sum of predicted values (could be used for filtering values below some threshold)
CreateFastLookupTable <- function(WordList,NgramNumber)
{
  n <- 0
  
  # print total rows to be processed.
  print(paste("Total rows to process:",length(WordList)))
  
  # create lookup table template
  Lookup <- data.table(Input="dummy",Pred1="dummy",Pred2="dummy",Pred3="dummy",ProbabilitySum =0)
  
  # if input is bigram call bigram predictor.
  if(NgramNumber==2){
    
    for(i in WordList)
    {
      temp <- BiGramNextWordPredictor(i,100)
      
      word_list <- temp[,.SD[1:3]]$Words
      
      ProbabilitySum <- sum(temp[,.SD[1:3]]$Probability,na.rm = TRUE)
      
      word_list <- c(i,word_list,round(ProbabilitySum,6))
      
      Lookup <- rbindlist(list(Lookup,as.list(word_list)))
      
      n<- n+1
      
      print(paste("Number of rows processed:",n))
    }
    
    return(Lookup)
  }
  else if(NgramNumber==3)# if input is trigram call trigram predictor.
  {
    for(i in WordList)
    {
      temp <- TriGramNextWordPredictor(i,100)
      
      word_list <- temp[,.SD[1:3]]$Words
      
      ProbabilitySum <- sum(temp[,.SD[1:3]]$Probability,na.rm = TRUE)
      
      word_list <- c(i,word_list,round(ProbabilitySum,6))
      
      Lookup <- rbindlist(list(Lookup,as.list(word_list)))
      
      n<- n+1
      
      print(paste("Number of rows processed:",n))
    }
    return(Lookup)
    
  }
  else if(NgramNumber==4) # if input is tetragram call tetragram predictor.
  {
    for(i in WordList)
    {
      temp <- TetraGramNextWordPredictor(i,100)
      
      word_list <- temp[,.SD[1:3]]$Words
      
      ProbabilitySum <- sum(temp[,.SD[1:3]]$Probability,na.rm = TRUE)
      
      word_list <- c(i,word_list,round(ProbabilitySum,6))
      
      Lookup <- rbindlist(list(Lookup,as.list(word_list)))
      
      n<- n+1
      
      print(paste("Number of rows processed:",n))
    }
    
    return(Lookup)
    
  }
  else{
    
    print("Incorrect NgramNumber values. Possible values are 2,3 and 4")
  }
  
}

##############################################################################################
##############################################################################################

#########Steps below create final lookup table###########################################

# create tetragram word list
WordList <- unique(tetragram$first_second_third)

# create lookup based on tetragram Input and save in db.
Lookup_4= CreateFastLookupTable(WordList,4)
dbInsert(db,"Lookup_4",Lookup_4)

setorder(Lookup_4,-ProbabilitySum)
Lookup_4 <- Lookup_4[,.SD[1:113671]]

WordList <- unique(trigram$first_second)




##################### start

WordList3 <- unique(trigram$first_second)

Lookup_3= CreateFastLookupTable(WordList3,3)

dbInsert(db,"Lookup_3",Lookup_3)

Lookup_3 <- Lookup_3[,.SD[2:118404]]

rm(Lookup_3)

WordList2 <- unique(bigram$first_word)

Lookup_2= CreateFastLookupTable(WordList2,2)

dbInsert(db,"Lookup_2",Lookup_2)

rm(Lookup_2)


###############################

Lookup_2 <- dbFetch(db,"Lookup_2")
Lookup_2 <- Lookup_2[,.SD[2:23540]]

Lookup_4 <- dbFetch(db,"Lookup_4")
Lookup_4 <-  Lookup_4[,.SD[2:113672]]

# Top 3 Unknown prediction
Lookup_unk <-data.table(Input="UNK",Pred1="the",Pred2="and",Pred3="to",ProbabilitySum =1)

PredictionLookup <- rbindlist(list(Lookup_4,Lookup_3,Lookup_2,Lookup_unk))

# remove probability sum as final data does not need filtering
PredictionLookup[,ProbabilitySum:=NULL]

setkey(PredictionLookup,Input)

saveRDS(PredictionLookup,file = "PredictionLookupRDS")

VocabList <- unigram[Frequency>50]

VocabList[,probability:=NULL]
VocabList[,adjusted_prob:=NULL]

# set key again
setkey(VocabList,Word)

# save vocabulary list
saveRDS(VocabList,file = "VocabListRDS")