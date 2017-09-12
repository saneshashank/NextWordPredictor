#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# load the required libraries
library(shiny)
library(quanteda)
library(data.table)
library(stringr)


# load the prediction lookup data
PredictionLookup <- readRDS("PredictionLookupRDS")

# read vocabulary list to be used for MLE prediction.
VocabList <- readRDS("VocabListRDS")

#'This funcrion predicts the most likely values
#'based on Maximum Likelihood Estimation values of words
#'stored in lookup dictionary, guesses the word you are 
#'trying to type
predict.MLE <- function(x)
{
  temp <- VocabList[grepl(paste0('^',x),Word)]
  if(!is.na(temp[,.SD[1]]$Word)){
    temp <- setorder(temp,-Frequency)
    temp <- temp[,.SD[1:3]]
    
    if(is.na(temp[,.SD[2]]$Word))
    {
      
      temp <- rbindlist(list(temp[,.SD[1]],VocabList[,.SD[1:2]]))
      
    }
    
    if(is.na(temp[,.SD[3]]$Word))
    {
      
      temp <- rbindlist(list(temp[,.SD[1:2]],VocabList[,.SD[1]]))
      
    }
    
    return(unlist(temp[,c(Word)]))
  }
  else
  {
    temp <- VocabList[,.SD[1:3]]
    return(unlist(temp[,c(Word)]))
    
  }
  
}

#'Predicts based on the precomputed values based on Stupid Backoff and KBO algorithm
#'stored in fast lookup tables.
#'
predict.baseline <- function(x)
{
  # convertto remove any no ascii words
  x <- iconv(x , from = "UTF-8", to = "ASCII", sub = " ")
  
  # tokenize text
  t <- tokenize(x,what="fasterword",remove_url = TRUE,remove_numbers =   TRUE, remove_punct = TRUE,remove_symbols = TRUE,remove_hyphens=TRUE,remove_twitter=TRUE, simplify=TRUE)
  
  # take last 3 words as input
  tailend <- tail(t,3)
  
  if(length(tailend)==3)
  {
    Y <- paste0(tail(t,3)[1],"_",tail(t,3)[2],"_",tail(t,3)[3])
  }
  else if(length(tailend)==2)
  {
    Y <- paste0("UNK","_",tail(t,3)[1],"_",tail(t,3)[2])
    
  }
  else if(length(tailend)==1)
  {
    Y <- paste0("UNK","_","UNK","_",tail(t,3)[1])
  }
  else
  {
    Y <- "_"
  }
  
  # checkin prediction lookup for matching tetragram
  result <- PredictionLookup[.(Y)]
  
  # if not found check for trigram and bigram
  if(is.na(result$Pred1))
  {
    
    Y <- paste0(str_split(Y,pattern="_")[[1]][2],"_",str_split(Y,pattern="_")[[1]][3])
    
    result <- PredictionLookup[.(Y)]
    
    if(is.na(result$Pred1))
    {
      Y <- str_split(Y,pattern="_")[[1]][2]
      
      result <- PredictionLookup[.(Y)]
      
      if(is.na(result$Pred1))
      {
        result <- PredictionLookup[.("UNK")]
        
        return(unlist(result[,c(Pred1,Pred2,Pred3)]))
      }
      else
      {
        return(unlist(result[,c(Pred1,Pred2,Pred3)]))
      }
    }
    else
    {
      return(unlist(result[,c(Pred1,Pred2,Pred3)]))
    }
  }
  else
  {
    return(unlist(result[,c(Pred1,Pred2,Pred3)]))
  }
  
}

# Define server logic for text prediction.
shinyServer(function(input, output, clientData, session) {
  
  # define global variable to store values in between sessions
  button_value <- "" 
  
  # last word value for MLE predictor
  MLE.last_word <- ""
  
  # observer to capture input text change
  observe({
    
    # store the typed text
    typed_txt <- char_tolower(input$InputText)
    
    # check the last typed entry
    last_entry <- substring(typed_txt, nchar(typed_txt), nchar(typed_txt))
    
    
    #' if the last entry is a word (not space), then use MLE based on last word
    #' dictionary for prediction.
    if(!grepl(" +",last_entry) & typed_txt != "")
    {
      x <- iconv(typed_txt , from = "UTF-8", to = "ASCII", sub = " ")
      
      t <- tokenize(x,what="fasterword",simplify=TRUE)
      
      tailend <- tail(t,1)
      
      
      # store in global var for possible replacement
      MLE.last_word <<- tailend
      
      
      o_label <- predict.MLE(tailend)
      
      
    }
    else # else use the prediction algorithm
    {
      
      o_label <- predict.baseline(typed_txt)
      
      # reset MLE last word value
      MLE.last_word <<- ""
    }
    
    #o_label <- predict.baseline(typed_txt)
    
    #' if the typed text is not entirely blank then display
    #' the predicted values.
    if(typed_txt != ""){
      # change label and value of Pred1 button
      updateActionButton(session,"Pred1",label = o_label[1])
      updateActionButton(session,"Pred2",label = o_label[2])
      updateActionButton(session,"Pred3",label = o_label[3])
    }
    
    # store predicted values in global variable defined.
    button_value <<- o_label
    
  }) # End of observer block
  
  # observe click of Pred1 button and assign selected value to InputText
  observeEvent(input$Pred1,{
    if(MLE.last_word==""){
      updateTextInput(session,"InputText", value = paste(input$InputText,button_value[1]))
    }
    else
    {
      x <- iconv(input$InputText , from = "UTF-8", to = "ASCII", sub = " ")
      
      replacementText <- substring(x,1,nchar(x)-nchar(MLE.last_word))
      
      updateTextInput(session,"InputText", value = paste0(replacementText,button_value[1]))
      
    }
  })
  
  # observe click of Pred2 button and assign selected value to InputText
  observeEvent(input$Pred2,{
    if(MLE.last_word==""){ 
      updateTextInput(session,"InputText", value = paste(input$InputText,button_value[2]))
    }
    else
    {
      x <- iconv(input$InputText , from = "UTF-8", to = "ASCII", sub = " ")
      
      replacementText <- substring(x,1,nchar(x)-nchar(MLE.last_word))
      
      updateTextInput(session,"InputText", value = paste0(replacementText,button_value[2]))
    }
  })
  
  # observe click of Pred3 button and assign selected value to InputText
  observeEvent(input$Pred3,{
    if(MLE.last_word==""){
      updateTextInput(session,"InputText", value = paste(input$InputText,button_value[3]))
    }
    else
    {
      x <- iconv(input$InputText , from = "UTF-8", to = "ASCII", sub = " ")
      
      replacementText <- substring(x,1,nchar(x)-nchar(MLE.last_word))
      
      updateTextInput(session,"InputText", value = paste0(replacementText,button_value[3]))
    }
  })
  
  
  # observe clear event.
  observeEvent(input$Clear,{
    
    #clear input text
    updateTextInput(session,"InputText", value ="")
    
    # change labels back
    updateActionButton(session,"Pred1",label = "Prediction 1")
    updateActionButton(session,"Pred2",label = "Prediction 2")
    updateActionButton(session,"Pred3",label = "Prediction 3")
  })
  
})
