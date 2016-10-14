library(shiny)


shinyUI(fluidPage(
  titlePanel("Map Plot and Word Cloud on tweets mentioning Presidential Candidates"),
  
  sidebarLayout(position = "right",
                sidebarPanel( "Words used in the tweets with more frequency"),
                mainPanel(h1("Map Plot"),
                          plotOutput("map"),
                          plotOutput("plot")
                          )
                          )
))


