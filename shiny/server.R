library(shiny)

shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi_graf(input$stevilo)
  })
})


narisi_graf = function(stevilo){
  if (stevilo == 2){
   izris(2)
  }
  else if (stevilo == 3){
    izris(3)
  }
  else if (stevilo == 5){
    izris(5)
  }
  else {
    print("NAPAKA")
  } 
  
} 
