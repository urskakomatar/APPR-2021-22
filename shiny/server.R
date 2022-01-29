library(shiny)


shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi_graf(input$drzava, input$starost.preb)
  })
})

narisi_graf = function(drzava, starost.preb){
  if (starost.preb == "+65"){
    graf = ggplot(Starost %>% filter(Država == drzava, starost == starost.preb)) +
      aes(x = leto, y = stevilo) +
      geom_col(position = "dodge", fill = "coral") +
      labs(
        x = "leto",
        y = "število",
        title = paste("glede na starost", drzava, sep = " ")
      )
    print(graf)
  } 
  else if (starost.preb == "15-24"){
    graf = ggplot(Starost %>% filter(Država == drzava, starost == starost.preb)) +
      aes(x = leto, y = stevilo) +
      geom_col(position = "dodge", fill = "coral") +
      labs(
        x = "leto",
        y = "število",
        title = paste("glede na starost", drzava, sep = " ")
      )
    print(graf)
  }
  else {
    print("NAPAKA")
  }
  

}
