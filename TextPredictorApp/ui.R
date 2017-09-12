#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#    reference: http://shiny.rstudio.com/gallery/update-input-demo.html 
#    https://github.com/rstudio/shiny-examples/tree/master/065-update-input-demo
#    https://shiny.rstudio.com/articles/scoping.html
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("JHU/Swiftkey Capstone Project - Text Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       br(),
       textInput("InputText",label = "Please enter text below",width = 500),
       
       #div(textOutput("OutputText", container = span), style=boxstyle),
       div(actionButton("Pred1",label="Prediction 1"),
           actionButton("Pred2",label="Prediction 2"),
           actionButton("Pred3",label="Prediction 3")),
       br(),
       actionButton("Clear",label="Clear"),
       br(),
       br(),
       strong("Prediction app instructions."),
       br(),
       p("Wait until the app is fully loaded (the input textbox and prediction boxes become visible), then just type in the textbox in english.Under the textbox, there are three boxes 
        (Prediction 1,Prediction 2 and Prediction3) in which the top 3 word suggestions would appear. When you are typing 
         a letter the most probable word would be shown,while once you hit space key the next predicted word would be shown.
         ."),
       p("You can choose any one of the suggestion by clicking on the desired word.
         Please note that while you are typing a word choosing the predicted word would replace the typed word, while if you choose 
         the predicted word after hitting the space key, the predicted word would be added to the typed text."),
       p("use Clear button to clear text and reset predictors."),
       p("Hope you enjoy the app!! thanks!!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      strong("About this App."),
      br(),
      p("This app has been developed as part of John's Hopkins University Data Science Specialization Capstone Project."),
      p("The app prediction engine has a repsonse time of 10.21 msec and a benchmark accuracy of 19.33%."),
      p("Be aware that slight slow response in app is due to the latency of Shiny server and not due to the actual app."),
      br(),
      p("Following two algorithms are used for prediction "),
      tags$ol(
        tags$li("Word completion suggestion is based on Maximum Likelihood estimation,"), 
        tags$li("Next word suggestion is based on interpolated Stupid Backoff and Katz Backoff Algorithms.")
      ),
      br(),
      p("The entire app has been developed keeping principles of reproducible research in mind, the repository for code is at:"),
      tags$a(href = "https://github.com/saneshashank/NextWordPredictor","https://github.com/saneshashank/NextWordPredictor"),
      p("where a detailed report on data analysis and prediction pipeline is present.")
      
    )
  )
))
