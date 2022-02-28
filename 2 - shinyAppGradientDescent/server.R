library(ggplot2)
library(shiny)
library(shinythemes)

function(input, output){
    
    funcX <- function(x){
        return(x**2 - 4*x + 10)
    }
    
    dFuncX <- function(x){
        return(2*x - 4)
    }
    
    gradientDescent <- function(derivativeFunction,
                                initialGuess,
                                stepMultiplier = 0.01,
                                precision = 0.0001,
                                maxIterations = 500){
        
        var1 <- initialGuess
        var2 <- NULL
        variableList <- c(var1)
        gradientList <- c(derivativeFunction(var1))
        
        for( i in 1:maxIterations){
            
            var2 <- var1
            gradient <- derivativeFunction(var1)
            
            var1 <- var1 - stepMultiplier * gradient
            
            variableList <- append(variableList, var1)
            
            gradientList <- append(gradientList, derivativeFunction(var1))
            
            
            stepSize <- abs(var1 - var2)
            
            if (stepSize <= precision){
                break
            }
        }
        
        return( list("x" = var1, "xValuesForIterations" = variableList, "slope" = gradientList))
    }
    
    dataList <- reactive({
        gradientDescent(derivativeFunction =  dFuncX,
                        initialGuess = input$initialGuessSlider,
                        stepMultiplier = input$stepMultiplierSlider,
                        precision = input$precisionSlider,
                        maxIterations = input$maxIterationsSlider)
    })
    
 

    
    x1 <- reactive({
        seq(from = -max(abs(dataList()$xValuesForIterations)), 
            to = max(abs(dataList()$xValuesForIterations)), 
            length.out = 100)
    })

             
    output$slopePlot <- renderPlot({
        ggplot() +
            geom_line(aes(x1(), dFuncX(x1())),
                      color = "blue",
                      alpha = 0.8,
                      size = I(2)) +
            xlab("x") +
            ylab("df(x)") +
            ggtitle("Slope of our cost function") +
            geom_point(aes(dataList()$xValuesForIterations, dataList()$slope),
                       color = "red",
                       alpha = 0.2,
                       size = I(4)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "violet", size = I(1))
    }, res = 96)
    
    
    output$functionPlot <- renderPlot({
        ggplot() +
            geom_line(aes(x1(), funcX(x1())),
                      color = "blue",
                      alpha = 0.8,
                      size = I(2)) +
            xlab("x") +
            ylab("f(x)") +
            ggtitle("Cost function") +
            geom_point(aes(dataList()$xValuesForIterations, funcX(dataList()$xValuesForIterations)),
                       color = "red",
                       alpha = 0.2,
                       size = I(4))
    }, res = 96)
    
    output$answer1 <- renderText({
        sprintf("Local minimum achieved at: %f", dataList()$x )
    })
    
    output$answer2 <- renderText({
        sprintf("Slope at this point: %f", tail(dataList()$slope, 1) )
    })
}