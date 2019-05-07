library(shiny)
library(shinythemes)
library(tidyverse)

# Define UI for random distribution app ----
ui <- fluidPage(
    
    tabsetPanel(type = "tabs",
                tabPanel("INT Continuous",
                         fluidRow(column(6, fluidRow(sliderInput("a", "Select Age:", 
                                                                 min = 12, max = 84, value = 30)),
                                            fluidRow(plotOutput("plotcont")),
                                            fluidRow(textOutput("ncont")),
                                            fluidRow(plotOutput("plotcontgt")),
                                            fluidRow(textOutput("ncontgt"))),
                                  column(6, fluidRow(sliderInput("acomp","Select Age:",
                                                                 min = 12, max = 84, value = 30)),
                                            fluidRow(plotOutput("plotcontcomp")),
                                            fluidRow(textOutput("ncontcomp")),
                                            fluidRow(plotOutput("plotcontcompgt")),
                                            fluidRow(textOutput("ncontcompgt"))))),
                tabPanel("INT Binary",
                         fluidRow(column(6, fluidRow(sliderInput("abin", "Select Age:", 
                                                                 min = 12, max = 84, value = 30)),
                                         fluidRow(plotOutput("plotbin")),
                                         fluidRow(textOutput("nbin")),
                                         fluidRow(plotOutput("plotbingt")),
                                         fluidRow(textOutput("nbingt"))),
                                  column(6, fluidRow(sliderInput("abincomp","Select Age:",
                                                                 min = 12, max = 84, value = 30)),
                                         fluidRow(plotOutput("plotbincomp")),
                                         fluidRow(textOutput("nbincomp")),
                                         fluidRow(plotOutput("plotbincompgt")),
                                         fluidRow(textOutput("nbincompgt")))))))

# Define server logic for random distribution app ----
server <- function(input, output) {
    
    scores <- read_csv("for_plots.csv")
    scores$totalbin <- as.integer(scores$total > 0)
    scores$sexch <- as.factor(scores$sexch)
    
    output$plotcont <- renderPlot({
        scores %>% 
            filter(Age <= input$a) %>% 
            ggplot(aes(x=total, y=100*(..count..)/sum(..count..), fill = sexch)) +
            geom_histogram(bins=21, position="dodge", alpha = .6) +
            labs(title = "INT total score for selected subjects by sex",
                 x = element_blank(), y = "%") +
            ylim(0,25) + 
            theme_bw()        
         })
    
    output$ncont <- renderText({
        n <- scores %>% filter(Age <= input$a) %>% count() %>% as.numeric()
        paste0("There are ", n, " subjects aged ", input$a, " yo or younger")
    })

    output$plotcontgt <- renderPlot({
        scores %>% 
            filter(Age > input$a) %>% 
            ggplot(aes(x=total, y=100*(..count..)/sum(..count..), fill = sexch)) +
            geom_histogram(bins=21, position="dodge", alpha = .6) +
            labs(title = "INT total score for selected subjects by sex",
                 x = element_blank(), y = "%") +
            ylim(0,25) + 
            theme_bw()        
    })
    
    output$ncontgt <- renderText({
        n <- scores %>% filter(Age <= input$a) %>% count() %>% as.numeric()
        ncomp <- nrow(scores) - n
        paste0("There are ", ncomp, " subjects older than ", input$a, " yo")
    })
    
    output$plotcontcomp <- renderPlot({
        scores %>% 
            filter(Age <= input$acomp) %>% 
            ggplot(aes(x=total, y=100*(..count..)/sum(..count..), fill = sexch)) +
            geom_histogram(bins=21, position="dodge", alpha = .6) +
            labs(title = "INT total score for selected subjects by sex",
                 x = element_blank(), y = "%") +
            ylim(0,25) + 
            theme_bw()        
    })
    
    output$ncontcomp <- renderText({
        n <- scores %>% filter(Age <= input$acomp) %>% count() %>% as.numeric()
        paste0("There are ", n, " subjects aged ", input$acomp, " yo or younger")
    })
    
    output$plotcontcompgt <- renderPlot({
        scores %>% 
            filter(Age > input$acomp) %>% 
            ggplot(aes(x=total, y=100*(..count..)/sum(..count..), fill = sexch)) +
            geom_histogram(bins=21, position="dodge", alpha = .6) +
            labs(title = "INT total score for selected subjects by sex",
                 x = element_blank(), y = "%") +
            ylim(0,25) + 
            theme_bw()        
    })
    
    output$ncontcompgt <- renderText({
        n <- scores %>% filter(Age <= input$acomp) %>% count() %>% as.numeric()
        ncomp <- nrow(scores) - n
        paste0("There are ", ncomp, " subjects older than ", input$acomp, " yo")
    })

    output$plotbin <- renderPlot({
        scores %>% 
            filter(Age <= input$abin) %>% 
            ggplot(aes(x=totalbin, y=100*(..count..)/sum(..count..), fill = sexch)) +
            geom_bar(position = "dodge", alpha = .6) +
            labs(title = "INT total score for selected subjects by sex",
                 x = element_blank(), y = "%") +
            #ylim(0,75) +
            theme_bw()        
    })
    
    output$nbin <- renderText({
        n <- scores %>% filter(Age <= input$abin) %>% count() %>% as.numeric()
        paste0("There are ", n, " subjects aged ", input$abin, " yo or younger")
    })
    
    output$plotbingt <- renderPlot({
        scores %>% 
            filter(Age > input$abin) %>% 
            ggplot(aes(x=totalbin, y=100*(..count..)/sum(..count..), fill = sexch)) +
            geom_bar(position = "dodge", alpha = .6) +
            labs(x = "INT binary score (0 vs 1+)", y = "%") +
            #ylim(0,75) +
            theme_bw()        
    })
    
    output$nbingt <- renderText({
        n <- scores %>% filter(Age > input$abin) %>% count() %>% as.numeric()
        ncomp <- nrow(scores) - n
        paste0("There are ", n, " subjects older than ", input$abin, " yo")
    })
    
    output$plotbincomp <- renderPlot({
        scores %>% 
            filter(Age <= input$abincomp) %>% 
            ggplot(aes(x=totalbin, y=100*(..count..)/sum(..count..), fill = sexch)) +
            geom_bar(position = "dodge", alpha = .6) +
            labs(title = "INT total score for selected subjects by sex",
                 x = element_blank(), y = "%") +
            #ylim(0,75) +
            theme_bw()        
    })
    
    output$nbincomp <- renderText({
        n <- scores %>% filter(Age <= input$abincomp) %>% count() %>% as.numeric()
        paste0("There are ", n, " subjects aged ", input$abincomp, " yo or younger")
    })
    
    output$plotbincompgt <- renderPlot({
        scores %>% 
            filter(Age > input$abincomp) %>% 
            ggplot(aes(x=totalbin, y=100*(..count..)/sum(..count..), fill = sexch)) +
            geom_bar(position = "dodge", alpha = .6) +
            labs(x = "INT binary score (0 vs 1+)", y = "%") +
            #ylim(0,75) +
            theme_bw()        
    })
    
    output$nbincompgt <- renderText({
        n <- scores %>% filter(Age <= input$abincomp) %>% count() %>% as.numeric()
        ncomp <- nrow(scores) - n
        paste0("There are ", ncomp, " subjects older than ", input$abincomp, " yo")
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)