library(shiny)
library(ggplot2)

setosaIris <- iris[iris$Species=="setosa",]
versicolorIris <- iris[iris$Species=="versicolor",]
virginicaIris <- iris[iris$Species=="virginica",]

setosaModel <- lm(Sepal.Width ~ Petal.Width, data=setosaIris)
versicolorModel <- lm(Sepal.Width ~ Petal.Width, data=versicolorIris)
virginicaModel <- lm(Sepal.Width ~ Petal.Width, data=virginicaIris)

shinyServer(
  function(input, output) {
    output$setosaModelCoeff <- renderPrint(setosaModel$coefficients)
    output$versicolorModelCoeff <- renderPrint(versicolorModel$coefficients)
    output$virginicaModelCoeff <- renderPrint(virginicaModel$coefficients)
    output$selectedSpecies <- renderPrint(input$species)
    output$selectedPetalWidth <- renderPrint(input$petalWidth)
    output$predictedSepalWidth <- renderText({
         if(input$species == "I.setosa") {
           setosaModel$coefficients[1] + (input$petalWidth * setosaModel$coefficients[2])
         }
         else if (input$species == "I.versicolor") {
           versicolorModel$coefficients[1] + (input$petalWidth * setosaModel$coefficients[2])
         }
         else if (input$species == "I.virginica") {
           virginicaModel$coefficients[1] + (input$petalWidth * setosaModel$coefficients[2])
         }
    })
    
    
    output$sepalPetalPlot <- renderPlot({
      qplot(Petal.Width,Sepal.Width,colour=Species,data=iris,xlab="Petal Width", ylab="Sepal Width")
    })
  }
)