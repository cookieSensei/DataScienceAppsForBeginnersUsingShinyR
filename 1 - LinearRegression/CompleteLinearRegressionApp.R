# Install the required libraries or package 

# Remove the hash # symbol in line 5 and execute it (ctrl + enter on windows or cmd + enter on mac) to install packages

# install.packages(c("shiny", "shinythemes", "dplyr", "ggplot2"))




#Loading essential libraries

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

# Load up the required dataset
dirtyDataset <- read.csv("resources/cost_revenue_dirty.csv")

# Check the top rows of dataset
dirtyDataset %>% head()

# Have a quick look at the dataset. Notics there are a lot of zeroes
# View(dirtyDataset) #uncomment

# Check column names
colnames(dirtyDataset)

# Keep only the required columns of data frame
dirtyDataset <- dirtyDataset[c(3,4,5)]

dirtyDataset %>% head()


# Edit column names to our preference in snakeCasing
colnames(dirtyDataset) <- c("movieTitle", "productionBudget", "worldwideGross")

dirtyDataset %>% head()


# Check for missing values
dirtyDataset[!complete.cases(dirtyDataset)] 
# That means there are no missing values 
# but there are a lot of false zeros in our dataset which could hinder our model

# Filter dataset rows where productionBudget is less than 1000
dirtyDataset[dirtyDataset$productionBudget < 1000, ] 
# something is wrong maybe these are strings

# Check structure of dataset
str(dirtyDataset)

# $ is a special symbol in R. to remove it, we use the pattern \\$
# using \\$ as pattern R will look for the dollar sign $ and replace it with the second argument 
# the second argument here is an empty string ""
dirtyDataset$productionBudget <- gsub(pattern = "\\$", "", dirtyDataset$productionBudget)

# We remove commas "," by replacing it with an empty string
dirtyDataset$productionBudget <- gsub(pattern = ",", "", dirtyDataset$productionBudget)

# Now we can safely convert the remaining string to numeric values
dirtyDataset$productionBudget <- as.numeric(dirtyDataset$productionBudget)

# Check the head 
dirtyDataset %>% head()

# Let's do the same for the worldwideGross column
dirtyDataset$worldwideGross <- gsub(pattern = "\\$", "", dirtyDataset$worldwideGross)
dirtyDataset$worldwideGross <- gsub(pattern = ",", "", dirtyDataset$worldwideGross)
dirtyDataset$worldwideGross <- as.numeric(dirtyDataset$worldwideGross)

# Structure
str(dirtyDataset)

# Check incomplete cases
dirtyDataset[!complete.cases(dirtyDataset),]

# Keep only the complete cases
dirtyDataset <- dirtyDataset[complete.cases(dirtyDataset),]

# Check the complete cases
dirtyDataset[complete.cases(dirtyDataset),]

# Check the incomplete cases
dirtyDataset[!complete.cases(dirtyDataset),]

dirtyDataset %>% head()

str(dirtyDataset)

# new problem arises
# there are zeros in our data

# Let's check if either of our numeric columns have zeros
# Filer using the vectorized "or" operator and check the head 
dirtyDataset[(dirtyDataset$productionBudget < 1000 | dirtyDataset$worldwideGross < 1000), ] %>% head()

# Keep only the rows where productionBudget and worldwideGross, both have more that 1000 value
# Its just to remove zeros
dirtyDataset[(dirtyDataset$productionBudget > 1000 & dirtyDataset$worldwideGross > 1000), ] %>% head()

# Save the new output as cleanDataset
cleanDataset <- dirtyDataset[(dirtyDataset$productionBudget > 1000 & dirtyDataset$worldwideGross > 1000), ]

cleanDataset %>% head()

# Lets check the minimum
min(cleanDataset$productionBudget)

min(cleanDataset$worldwideGross)

# Check for incomplete rows
cleanDataset[!complete.cases(cleanDataset),]

# Lets call it finalDataset as our data cleaning is complete
finalDataset <- cleanDataset

# Remove cleanDataset and dirtyDataset varaibles
rm(cleanDataset)
rm(dirtyDataset)

# Lets look at the finalDataset again to see if it is in the format we want it to be
# Column names have snakeCasing, there are no zeros, or missing values, great
finalDataset %>% head()





############



finalDataset %>% head()

# Lets divide our numeric columns by 1 million 
finalDataset$productionBudget <- finalDataset$productionBudget / 1e6
finalDataset$productionBudget

finalDataset$worldwideGross <- finalDataset$worldwideGross / 1e6



# Lets create a regressor for our data
# Use the help guide on lm() for check what arguments it uses
?lm()

# formula = y axis is directly proportional to x axis
movieReg <- lm(data = finalDataset, formula = worldwideGross ~ productionBudget)

# Check the slope (m) and intercept (y) on our equation y = mx+c
movieReg

# Now lets predict the y axis using an x axis
# Here we use productionBudget column and predict what y values (worldwideGross) our regressor movieReg provides
predictedGross <- predict(movieReg, data = finalDataset$productionBudget)

# Lets do a sample plot first before we head to make a reactive app on it

# Create a scatterplot with productionBudget on x axis and worldwideGross on y axis
# Later we add a layer of geom_line
# The line plot has productionBudget on x axis and predicted y values (predictedGross) on y axis
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

# Call the plot
regressionPlot

# Check the stats on regressor
summary(movieReg)
# The R square value is almost 55% which means our predictions using linear regression are 55% accurate
# This is a good accuracy given that we are only using budget to predict revenue
# There are other variables which could enhance our prediction
# For example - movie production time, actors, place of production, directors and so on

finalDataset %>% head()


# Check the number of rows on our dataset
nrow(finalDataset) # 5023 rows


# Look at the tail 
finalDataset %>% tail()
# There is a mismatch between number of rows and row index numbers
# There are 5023 rows but dataset ends with 5383

# lets reset row names now
rownames(finalDataset) <- 1:nrow(finalDataset)

nrow(finalDataset)
finalDataset %>% tail()


#######################
###############  SHINY APP
#######################



# User interface of our app
ui <- fluidPage(
    
    theme = shinytheme("united"), # Theme used. We could also use bootstrap themes here.
    
    tabsetPanel( # Hit F1 on function for more info
        
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
                tableOutput("tableBrush")
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
    
    finalDatasetReactive <- reactive({
        finalDataset$sel <- selected()
        finalDataset
    })
    
    output$ourPlot <- renderPlot({
        
        finalDatasetReactive() %>% ggplot(aes(productionBudget, worldwideGross)) +
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
    
    output$tableBrush <- renderTable({
        finalDatasetReactive()[finalDatasetReactive()$sel == "TRUE",] %>% select(movieTitle, productionBudget, worldwideGross)
    })

}

shinyApp(ui, server)
