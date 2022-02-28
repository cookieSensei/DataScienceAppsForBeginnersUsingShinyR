library(shiny)
library(shinythemes)
library(ggplot2)

fluidPage(
    theme = shinytheme("united"),
    
    titlePanel("Gradient descent algorithm for one variable"),
    
    HTML("<h3><center>cost function: f(x) = x^2 - 4x + 10</center></h3>"),
    
    tabsetPanel(
        tabPanel(
            "Calculations",
            
            HTML("<h3><center>Change the values of parameters to see how gradient descent works</h3><center>"),
            
            sidebarLayout(
                
                sidebarPanel(
                    
                    sliderInput("initialGuessSlider", "Initial Guess", 
                                min = -50, max = 50, value=10),
                    sliderInput("stepMultiplierSlider",label = "Learning Rate",
                                min = 0.0001, max = 1.1, value = 0.01),
                    sliderInput("precisionSlider", label = "Precision (keep it low for max accuracy)",
                                min = 0.00001, max = 0.001, value = 0.0001),
                    sliderInput("maxIterationsSlider", label = "Max number of iterations",
                                min = 10, max = 500, value = 100)
                ),
                mainPanel(
                    fluidRow(
                        plotOutput("functionPlot"),
                    ),
                    fluidRow(
                        plotOutput("slopePlot")
                    )
                )
            ),
            fluidRow(
                verbatimTextOutput("answer1")
            ),
            fluidRow(
                verbatimTextOutput("answer2")
            )
            
        ),
        
        tabPanel("Documentation guide",
                 "Fiddle around with different values to see how the algorith works. ",
                 "Notice that as the learning rate is increased algorithm breaks")
    )
)