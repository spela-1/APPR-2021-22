library(shiny)

shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(sidebarPanel(
                  selectInput(
                    "stevilo",
                    label = "Število skupin:",
                    choices = c(2, 3, 5),
                    selected = 2
                  ))
                ,
                mainPanel(plotOutput("graf"))),
  uiOutput("izborTabPanel")))

