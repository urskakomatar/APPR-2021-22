library(shiny)


shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi_graf(input$drzava, input$starost.preb)
  })
})

narisi_graf = function(drzava, starost.preb){
  if (starost.preb == "+65"){
    graf = ggplot(Starost %>% filter(Država == drzava, starost == starost.preb)) +
      aes(x = leto, y = stevilo / 10^3) +
      geom_col(position = "dodge", fill = "coral") +
      labs(
        x = "Leto",
        y = "Število potovanj v tisočih",
        title = paste("Število potovanj glede na starostno skupino", drzava, sep = " ")
      )
    print(graf)
  } 
  else if (starost.preb == "15-24"){
    graf = ggplot(Starost %>% filter(Država == drzava, starost == starost.preb)) +
      aes(x = leto, y = stevilo/ 10^3) +
      geom_col(position = "dodge", fill = "coral") +
      labs(
        x = "Leto",
        y = "Število potovanj v tisočih",
        title = paste("Število potovanj glede na starostno skupino", drzava, sep = " ")
      )
    print(graf)
  }
  else if (starost.preb == "25-44"){
    graf = ggplot(Starost %>% filter(Država == drzava, starost == starost.preb)) +
      aes(x = leto, y = stevilo/ 10^3) +
      geom_col(position = "dodge", fill = "coral") +
      labs(
        x = "Leto",
        y = "Število potovanj v tisočih",
        title = paste("Število potovanj glede na starostno skupino", drzava, sep = " ")
      )
    print(graf)
  }
  else if (starost.preb == "45-65"){
    graf = ggplot(Starost %>% filter(Država == drzava, starost == starost.preb)) +
      aes(x = leto, y = stevilo/ 10^3) +
      geom_col(position = "dodge", fill = "coral") +
      labs(
        x = "Leto",
        y = "Število potovanj v tisočih",
        title = paste("Število potovanj glede na starostno skupino", drzava, sep = " ")
      )
    print(graf)
  }
  else {
    print("NAPAKA")
  }
  

}

