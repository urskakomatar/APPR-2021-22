library(shiny)

shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput(
                    "starost.preb",
                    label = "Starost:",
                    choices = c("+65","15-24"),
                    selected = "15-24"
                  ),
                  selectInput(
                    "drzava",
                    label = "država:",
                    choices = average_indeks$Država,
                    selected = "Slovenija"
                  ))
                ,
                mainPanel(plotOutput("graf"))),
  uiOutput("izborTabPanel")))
