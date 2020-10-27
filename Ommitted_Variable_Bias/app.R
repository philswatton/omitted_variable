#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(faux)
library(tidyverse)
library(texreg)


omit_graph <- function (corrxz = 0.4, intercept = 5, bx = 2, bz = 2) {
    
    data <- rnorm_multi(n = 1000,
                        vars = 3,
                        mu = c(0,0,0),
                        sd = c(1,1,1),
                        r = c(1,corrxz,0,
                              corrxz,1,0,
                              0,0,1),
                        varnames = c("x","z","u"))
    
    data$y <- intercept + bx*data$x + bz*data$z + data$u
    
    model1 <- y ~ x
    model2 <- y ~ x + z
    
    modelList <- list(model1, model2)
    
    modelOut <- map(modelList, ~lm(.x,data=data))
    
    
    summary1 <- summary(modelOut[[1]])
    summary2 <- summary(modelOut[[2]])
    
    
    out <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_point(alpha = 0) + 
        
        geom_abline(intercept = intercept, slope = bx, size = 1, color = "orange") +
        
        geom_abline(intercept = summary1$coefficients[1,1], slope = summary1$coefficients[2,1], size = 1, color = "red") +
        
        geom_abline(intercept = summary2$coefficients[1,1], slope = summary2$coefficients[2,1], size = 1, color = "blue") +
        
        xlim(0,NA) +
        ylim(0,NA) +
        
        
        annotate("text",
                 x = 2,
                 y = summary1$coefficients[1,1] + 2 * summary1$coefficients[2,1],
                 #angle = atan(summary1$coefficients[2,1] * data$x_to_y)  * 180/pi,
                 label = str_c("Biased Estimate: ", round(summary1$coefficients[2,1], digits=2))) +
        
        
        annotate("text",
                 x = 2,
                 y = summary2$coefficients[1,1] + 2 * summary2$coefficients[2,1],
                 #angle = atan(summary1$coefficients[2,1] * data$x_to_y)  * 180/pi,
                 label = str_c("Unbiased Estimate: ", round(summary2$coefficients[2,1], digits=2))) +
        theme_bw()
    
    return(out)
}

omit_graph(corrxz=0.5, bx=2, bz=2)



data <- rnorm_multi(n = 1000,
                    vars = 3,
                    mu = c(0,0,0),
                    sd = c(1,1,1),
                    r = c(1,0.4,0,
                          0.4,1,0,
                          0,0,1),
                    varnames = c("x","z","u"))

data$y <- 5 + 2*data$x + 2*data$z + data$u # E[bx] = 2 + 0.4*2 = 2.8 if z is included
data <- data %>%
    mutate(x_to_y = (range(x)[2] - range(x)[1])/(range(y)[2] - range(y)[1]))


model1 <- y ~ x
model2 <- y ~ x + z

modelList <- list(model1, model2)

modelOut <- map(modelList, ~lm(.x,data=data))


summary1 <- summary(modelOut[[1]])
summary2 <- summary(modelOut[[2]])


ggplot(data = data, mapping = aes(x = x, y = y)) +
    geom_point(alpha=0) + 
    
    geom_abline(intercept = 5, slope = 2, size = 1, color = "orange") +
    
    geom_abline(intercept = summary1$coefficients[1,1], slope = summary1$coefficients[2,1], size = 1, color = "red") +

    geom_abline(intercept = summary2$coefficients[1,1], slope = summary2$coefficients[2,1], size = 1, color = "blue") +

    xlim(0,NA) +
    ylim(0,NA) +
    
    
    annotate("text",
             x = 2,
             y = summary1$coefficients[1,1] + 2 * summary1$coefficients[2,1],
             #angle = atan(summary1$coefficients[2,1] * data$x_to_y)  * 180/pi,
             label = str_c("Biased Estimate: ", round(summary1$coefficients[2,1], digits=2))) +
    
    
    annotate("text",
             x = 2,
             y = summary2$coefficients[1,1] + 2 * summary2$coefficients[2,1],
             #angle = atan(summary1$coefficients[2,1] * data$x_to_y)  * 180/pi,
             label = str_c("Unbiased Estimate: ", round(summary2$coefficients[2,1], digits=2))) +
    theme_bw()







# Define UI for application that draws a histogram
ui <- fluidPage(
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
