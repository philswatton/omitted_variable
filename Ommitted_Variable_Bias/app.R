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
library(DiagrammeR)


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
        
        #geom_abline(intercept = intercept, slope = bx, size = 1, color = "orange") +
        
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


grViz(str_c("digraph {

  # a 'graph' statement
  graph [layout = dot,
       rankdir = LR,
  overlap = true,
  fontsize = 10]

  # several 'node' statements
  node [shape = circle]
  X, Y, Z

  # several 'edge' statements
  X->Y[label = 'bx: ",1,"', color = red, fontcolor = red]
  Z->Y[label = 'bz: ",1,"', color = orange, fontcolor = orange]
  X->Z[dir=both, label = 'corr(x,z): ",0.5,"', color = blue, fontcolor = blue]
}"))


# biased result: E[bx] = bx + c * bz


# Define frontend UI for application
ui <- fluidPage(
  title = 'Ommitted Variable Bias',
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")), #with thanks to https://stackoverflow.com/questions/54876731/inline-latex-equations-in-shiny-app-with-mathjax
  
  
  titlePanel("Ommitted Variable Bias"),
  sidebarLayout(
    sidebarPanel(
      numericInput("bx", withMathJax("$B_x$"),value=1),
      numericInput("bz", withMathJax("$B_z$"),value=1),
      numericInput("b0", withMathJax("$B_0$"),value=0),
      numericInput("corrXZ",withMathJax("$Correlation(x,z)$"), min=-0.99, max=0.99,value=0.5),
      
      uiOutput(outputId = "formulas")
    ),
    mainPanel(
      grVizOutput(outputId = "diagram", width="500px", height="200px"),
      plotOutput(outputId = "biasPlot", width="500px", height="500px")
    )
  )
)


# Define backend server logic for application
server <- function(input, output, session) {
    
    output$formulas <- renderUI({
        withMathJax(
            helpText("The 'true' formula here is always $y=B_0 + B_xx + B_zz$. Recall that $B_x$ is the effect of x on y and $B_z$ is the effect of confounder z on y. $B_0$ is the 'intercept' - or the value that y takes on when both x and z are 0. You can ignore this for now."),
            
            helpText(str_c("At the moment, given the inputs, the formula for y is $y=",input$b0,"+",input$bx,"*x+",input$bz,"*z$.")),
            
            helpText(str_c("When we estimate the value of $B_x$, we denote the estimator $b_x$. When our estimate doesn't suffer from ommitted variable bias, it should be an unbiased estimate of $B_x$. So we say the expected value of $b_x$ is $E[b_x]=",input$bx,"$.")),
            
            helpText("However, if there is a confounding variable $z$ that both has an effect on $y$ and correlates with $x$, then failure to include this will result in bias for our estimate $b_x$"),
            
            helpText("We can calculate the expected value of $b_x$ with the formula $E[b_x] = B_x + c * B_z$, where $c$ is the correlation between $x$ and $z$."),
            
            helpText(str_c("So, the expected value of $b_1$ given the current inputs is $E[b_x] = ",input$bx,"+",input$corrXZ,"*",input$bz,"=",(input$bx+input$corrXZ*input$bz),"$.")),
            
            helpText("The graph to the right is produced by simulating data based on the above inputs, then fitting an unbiased model that includes both x and z, and a 'biased' model that only includes x."),
            
            helpText("Of course, the bias depends entirely on $c * B_z$ - if one of these components is 0, the 'biased' estimate will also be ubiased! Try this now."),
            
            helpText("Try seeing what happens to the amount of bias with different combinations of $c$ and $B_z$ - since each one can be either positive, negative, or 0, there are 9 total combinations of which 4 will produce bias."),
            
            helpText("Once you're feeling a bit more comfortable with what's happening, remember that an estimate is an 'overstimate' if it biased away from 0, and an 'underestimate' if it is biased towards 0. Make $B_x$ negative and see how the same kinda of bias can become under or over-estimates depending on the direction of $B_x$."))
    })
    
    output$diagram <- renderGrViz({
      grViz(str_c("digraph {

  # a 'graph' statement
  graph [layout = dot,
       rankdir = LR,
  overlap = true,
  fontsize = 10]

  # several 'node' statements
  node [shape = circle]
  X, Y, Z

  # several 'edge' statements
  X->Y[label = 'Bx: ",input$bx,"', color = red, fontcolor = red]
  Z->Y[label = 'Bz: ",input$bz,"', color = orange, fontcolor = orange]
  X->Z[dir=both, label = 'Corr(x,y): ",input$corrXZ,"', color = blue, fontcolor = blue]
}"))
        })
    
    output$biasPlot <- renderPlot({
        data <- rnorm_multi(n = 1000,
                            vars = 3,
                            mu = c(0,0,0),
                            sd = c(1,1,1),
                            r = c(1,input$corrXZ,0,
                                  input$corrXZ,1,0,
                                  0,0,1),
                            varnames = c("x","z","u"))
        
        data$y <- input$b0 + input$bx*data$x + input$bz*data$z + data$u
        
        model1 <- y ~ x
        model2 <- y ~ x + z
        
        modelList <- list(model1, model2)
        
        modelOut <- map(modelList, ~lm(.x,data=data))
        
        
        summary1 <- summary(modelOut[[1]])
        summary2 <- summary(modelOut[[2]])
        
        
        out <- ggplot(data = data, mapping = aes(x = x, y = y)) +
            geom_point(alpha = 0) + 
            
            #geom_abline(intercept = intercept, slope = bx, size = 1, color = "orange") +
            
            geom_abline(intercept = summary1$coefficients[1,1], slope = summary1$coefficients[2,1], size = 1, color = "red") +
            
            geom_abline(intercept = summary2$coefficients[1,1], slope = summary2$coefficients[2,1], size = 1, color = "blue") +
            
            xlim(0,NA) +
            #ylim(0,NA) +
            
            
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
        
        out
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
