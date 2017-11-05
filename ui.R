#install.packages("shiny")
library("shiny")
options(shiny.maxRequestSize=9*1024^2)

#install.packages("igraph")
#install.packages("network") 
#install.packages("sna")
#install.packages("ndtv")
#install.packages("extrafont")


#library(igraph)
#library(network)
#library(sna)
#library(ndtv)
#library(extrafont)

#install.packages("networkD3")
#library(networkD3)



shinyUI(fluidPage(
  
  titlePanel(title=h3("Brain Informatics using Deep Learning", align="center")),
  sidebarLayout(
    
    sidebarPanel(
      h5("Powered By:"),
      tags$img(src="logo.gif", height = 100, width=100),
      br(),
      h4(helpText("Select the number of channels:-")), 
      checkboxInput("f14", "14 channels", value = FALSE),
      checkboxInput("f64", "64 channels", value = FALSE),
      checkboxInput("f256", "256 channels", value = FALSE),
      fileInput("file", "Upload the Training Dataset"),
      fileInput("filealpha", "Upload the alpha dataset for testing correlation"),
      fileInput("filebeta", "Upload the beta dataset for testing correlation"),
      fileInput("filegamma", "Upload the gamma dataset for testing correlation"),
      fileInput("filetheta", "Upload the theta dataset for testing correlation"),
      br(),
      h4(helpText("Upload the 64 values of the data for which you want to predict the class: (for 64 channel only)")),
      fileInput("filetest", "Upload the 64 channel data to test the models"),
      
      tags$hr(),
      h4(helpText("Choose the Band for which you are entering values below: (For prediction)")),
      checkboxInput("alpha", "Alpha", value = TRUE),
      checkboxInput("beta", "Beta", value = FALSE),
      checkboxInput("gamma", "Gamma", value = FALSE),
      checkboxInput("theta", "Theta", value = FALSE),
      br(),
      h4(helpText("Check below if your training dataset contains headers")),
      checkboxInput("header", "Header", TRUE),
      checkboxInput("stringsasfactors", "StringsAsFactors", FALSE),
      br(),
      conditionalPanel(condition = "input.f14 == TRUE",
      h4(helpText("Enter the values of 14-channel EEG Signals for testing:-")),
      textInput("c1", "Enter the value for AF3", value = 3.609134, width = '200px'),
      textInput("c2", "Enter the value for F7", value = 0.428535, width = '200px'),
      textInput("c3", "Enter the value for F3", value = 1.288644, width = '200px'),
      textInput("c4", "Enter the value for FC5", value = 1.820404, width = '200px'),
      textInput("c5", "Enter the value for T7", value = 0.283729, width = '200px'),
      textInput("c6", "Enter the value for P7", value = 0.293612, width = '200px'),
      textInput("c7", "Enter the value for O1", value = 1.157706, width = '200px'),
      textInput("c8", "Enter the value for O2", value = 1.728738, width = '200px'),
      textInput("c9", "Enter the value for P8", value = 1.068931, width = '200px'),
      textInput("c10", "Enter the value for T8", value = 0.883266, width = '200px'),
      textInput("c11", "Enter the value for FC6", value = 1.240695, width = '200px'),
      textInput("c12", "Enter the value for F4", value = 2.452375, width = '200px'),
      textInput("c13", "Enter the value for F8", value = 0.072686, width = '200px'),
      textInput("c14", "Enter the value for AF4", value = 1.446363, width = '200px')),
      br(),
      radioButtons("sep", "Seperator", choices=c(Comma=",", Semicolon=";", Tab="\t", Space=" "), selected=","),
      br(),            
      
      submitButton("Run")
    ),
    
    
    mainPanel(uiOutput("tb"))               
    
    
  )
  
)
)
