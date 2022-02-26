library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

dirtyDataset <- read.csv("resources/cost_revenue_dirty.csv")

dirtyDataset %>% head()

colnames(dirtyDataset)

dirtyDataset <- dirtyDataset[c(3,4,5)]

dirtyDataset %>% head()

colnames(dirtyDataset) <- c("movieTitle", "productionBudget", "worldwideGross")

dirtyDataset %>% head()

dirtyDataset[!complete.cases(dirtyDataset)] 
# That means there are no missing values 
# but there are a lot of false zeros in our dataset which could hinder our model

dirtyDataset[dirtyDataset$productionBudget < 1000, ] 
# something is wrong maybe these are strings

str(dirtyDataset)

dirtyDataset$productionBudget <- gsub(pattern = "\\$", "", dirtyDataset$productionBudget)
dirtyDataset$productionBudget <- gsub(pattern = ",", "", dirtyDataset$productionBudget)
dirtyDataset$productionBudget <- as.numeric(dirtyDataset$productionBudget)

dirtyDataset %>% head()

dirtyDataset$worldwideGross <- gsub(pattern = "\\$", "", dirtyDataset$worldwideGross)
dirtyDataset$worldwideGross <- gsub(pattern = ",", "", dirtyDataset$worldwideGross)
dirtyDataset$worldwideGross <- as.numeric(dirtyDataset$worldwideGross)

str(dirtyDataset)

dirtyDataset[!complete.cases(dirtyDataset),]

dirtyDataset <- dirtyDataset[complete.cases(dirtyDataset),]

dirtyDataset[complete.cases(dirtyDataset),]

dirtyDataset[!complete.cases(dirtyDataset),]

dirtyDataset %>% head()

str(dirtyDataset)

# new problem arises
# there are zeros in our data

dirtyDataset[(dirtyDataset$productionBudget < 1000 | dirtyDataset$worldwideGross < 1000), ] %>% head()

dirtyDataset[(dirtyDataset$productionBudget > 1000 & dirtyDataset$worldwideGross > 1000), ] %>% head()

cleanDataset <- dirtyDataset[(dirtyDataset$productionBudget > 1000 & dirtyDataset$worldwideGross > 1000), ]

cleanDataset %>% head()

min(cleanDataset$productionBudget)

min(cleanDataset$worldwideGross)

cleanDataset[!complete.cases(cleanDataset),]

finalDataset <- cleanDataset

rm(cleanDataset)
rm(dirtyDataset)

finalDataset %>% head()





############



finalDataset %>% head()

finalDataset$productionBudget <- finalDataset$productionBudget / 1e6
finalDataset$productionBudget

finalDataset$worldwideGross <- finalDataset$worldwideGross / 1e6

movieReg <- lm(data = finalDataset, formula = worldwideGross ~ productionBudget)

movieReg

predictedGross <- predict(movieReg, data = finalDataset$productionBudget)


regressionPlot <- finalDataset %>% ggplot(aes(productionBudget, worldwideGross)) +
    geom_point(color = "blue", alpha = 0.2) +
    geom_line(
        aes(
            productionBudget, 
            predictedGross,
            color = "red",
            alpha = 0.8,
            size = I(1.5)
        ), 
        linetype = "dashed",
        show.legend = FALSE
    ) +
    labs(x = "Production budget of movie (in millions)",
         y = "Worldwide Gross (in millions)") 

# regressionPlot

summary(movieReg)


finalDataset %>% head()



nrow(finalDataset)
finalDataset %>% tail()

    # lets reset row names now

rownames(finalDataset) <- 1:nrow(finalDataset)

nrow(finalDataset)
finalDataset %>% tail()


#######################





ui <- fluidPage(
    
    theme = shinytheme("united"),
    
    tabsetPanel(
        
        tabPanel(
            "Reactive plot",
            
            fluidRow(
                column(
                    8,
                    HTML("<h3><center>For info on specific data points</center></h3>"),
                    
                    HTML("<h3><center>Select area on plot</center></h3>"),
                    
                    HTML("<h4><center>Double click on plot to reset brushed / selected points</center></h4>"),
                    
                    plotOutput("ourPlot", 
                               width = "900px", 
                               brush = "brushID",
                               dblclick = "doubleClickID")
                )
            ),
            
            fluidRow(
                column(
                    10,
                    verbatimTextOutput("brushOutput")
                )
            )
        ),
        
        tabPanel(
            "Regression Summary",
            fluidRow(
                
                column(
                    8,
                    HTML("<h1><center>Linear Regression</center></h1>"),
                    
                    HTML("<h3>Summary of regression</h3>"),
                    
                    verbatimTextOutput("summaryOut"),
                )
            )
        )
    )
)

server <- function(input, output){
    
    selected <- reactiveVal(rep(FALSE, nrow(finalDataset)))
    
    observeEvent(
        input$brushID,
        {
            brushed <- brushedPoints(finalDataset, 
                                     input$brushID,
                                     allRows = TRUE)$selected_
            selected(brushed | selected())
        }
    )
    
    observeEvent(
        input$doubleClickID,
        {
            selected(rep(FALSE, nrow(finalDataset)))
        }
    )
    
    output$summaryOut <- renderPrint(
        summary(movieReg)
    )
    
    output$ourPlot <- renderPlot({
        
        finalDataset$sel <- selected()
        
        finalDataset %>% ggplot(aes(productionBudget, worldwideGross)) +
            geom_point(aes(colour = sel), alpha = 0.4) +
            scale_color_discrete(limits = c("TRUE", "FALSE"))+
            geom_line(
                aes(
                    productionBudget, 
                    predictedGross,
                    color = "red",
                    alpha = 0.8,
                    size = I(1.5)
                ), 
                linetype = "dashed",
                show.legend = FALSE
            ) +
            labs(x = "Production budget of movie (in millions)",
                 y = "Worldwide Gross (in millions)") 

    }, res = 96)
    
    output$brushOutput <- renderPrint({
        finalDataset$sel <- selected()
        finalDataset[finalDataset$sel == "TRUE",]
        })
}

shinyApp(ui, server)