library(shiny)

shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput(
                    "starost.preb",
                    label = "Starost:",
                    choices = c("15-24","25-44","45-65","+65"),
                    selected = "15-24"
                  ),
                  selectInput(
                    "drzava",
                    label = "Država:",
                    choices = average_indeks$Država,
                    selected = "Slovenija"
                  ))
                ,
                mainPanel(plotOutput("graf"))),
  uiOutput("izborTabPanel")))
