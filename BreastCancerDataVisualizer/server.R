#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(ggthemes)
library(tidyverse)
library(corrplot)
library(Rtsne)
library(ggcorrplot)
library(gridExtra)
library(grid)
library(reshape2)
library(psych) # factor analysis

cancerDataset <- read.csv("data.csv")

sampleCancer <- cancerDataset

rows <- sample(nrow(sampleCancer))

sampleCancer <- sampleCancer[rows, ]

sampleCancer$Index = as.numeric(row.names(sampleCancer))


shinyServer(function(input, output) {
  
  
    dataSample <- reactive({
      #req(input$x)
      #sampleCancer[names(sampleCancer) %in% input$x, input$sampleSize]
      sampleCancer[sample(nrow(sampleCancer),input$sampleSize),]
    })
    
    output$plot <- renderPlotly({
      
      xInput <- input$x
      yInput <- input$y
      
      if(input$graph %in% "Scatter Plot"){
        
        p <- ggplot(data = dataSample(), aes_string(x = xInput , y = yInput, col = "Diagnosis")) + 
          geom_point() +
          geom_smooth() +
          #geom_line(size = 0.5) +
          #geom_hline(aes(yintercept = as.numeric(x)), linetype = 'dashed', color = 'red')+
          #geom_vline(aes(xintercept = as.numeric(y)), linetype = 'dashed', color = 'red')+
          labs(title = paste(xInput, "and", yInput),
               x = xInput, y = yInput) +
          theme_gdocs()
        

      }else if(input$graph %in% "Histogram"){
        if(xInput %in% "Diagnosis"){
          stop("Cannot plot histogram for Diagnosis. Please choose other parameter.")
        }
        
        p <- ggplot(dataSample(), aes_string(x = xInput)) +
          geom_histogram(fill = "#69b3a2") +
          ggtitle(paste("Histogram of ", xInput))
        
      }else if(input$graph %in% "Bar Graph"){
        
        ggplot(dataSample(), aes_string(x = xInput, fill = "Diagnosis")) +
          geom_bar(stat = "count", position = "stack", show.legend = TRUE) +
          ggtitle(paste("Bar Graph of ", xInput))
        
      }else if(input$graph %in% "Box Plot"){
        
        cancerDataset1 <- melt(dataSample()[2:32], id = "Diagnosis")
        
        ggplot(cancerDataset1, aes(x = xInput, y = value, color = Diagnosis)) +
          #geom_jitter(aes(color = Diagnosis)) +
          geom_boxplot() +
          labs(x = NULL, y = "Standardized Value", title = paste("Box Plot of", xInput))
        }
    })
    
    output$corr <- renderPlot({
      
      cancerDataset$Diagnosis <- factor(cancerDataset$Diagnosis, levels = c("M", "B"), labels = c(0, 1))
      cancerDataset$Diagnosis <- as.numeric(as.character(cancerDataset$Diagnosis))
      #options(repr.plot.width = 20, repr.plot.height = 20, repr.plot.res = 100)
      #str(cancerDataset)
      
      #cancerDataset <- cancerDataset %>% relocate(Diagnosis, .after = fractal_dimension_worst)
      
      r <- cor(cancerDataset)
      
      round(r, 2)
      
      #ggcorrplot(r, 
      # method = "circle",
      #title = "Correlation between Variables",
      #colors = c("#6D9EC1", "white", "#E46726"),
      #outline.col = "white",
      #ggtheme = ggplot2::theme_light,
      #hc.order = TRUE,
      #lab = FALSE,
      #size = 5,
      #type = "lower")
      
      #ggcorr(cancerDataset[, 2:32], 
      #method = c("everything", "pearson"),
      #geom = "text",
      #nbreak = 5,
      #palette = "RdYlBu",
      #size = 5,
      #hjust = 1,
      #layout.exp = 1,
      #label = TRUE,
      #label_alpha = 0.5)
      
      par(mfrow = c(1,3))
      p1 <- corrplot(cor(cancerDataset[, c(2:12)]),
                     method = "circle",
                     order = "hclust",
                     type = "lower",
                     diag = FALSE,
                     tl.col = "black",
                     addCoef.col = "pink",
                     number.cex = 0.9,
                     bg = "white",
                     title = "Correlation between Variables",
                     mar = c(0, 0, 5, 0)
                     )
      
      p2 <- corrplot(cor(cancerDataset[, c(12:22, 2)]),
                     method = "circle",
                     order = "hclust",
                     type = "lower",
                     diag = FALSE,
                     tl.col = "black",
                     addCoef.col = "pink",
                     number.cex = 0.9,
                     bg = "white",
                     title = "Correlation between Variables",
                     mar = c(0, 0, 5, 0))

      p3 <- corrplot(cor(cancerDataset[, c(22:32, 2)]),
                     method = "circle",
                     order = "hclust",
                     type = "lower",
                     diag = FALSE,
                     tl.col = "black",
                     addCoef.col = "pink",
                   number.cex = 0.9,
                     bg = "white",
                     title = "Correlation between Variables",
                     mar = c(0, 0, 5, 0))
    
      
    })


    output$scatter <- renderPlotly({
      
      xInput <- input$x
      yInput <- input$y
    

      p <- ggplot(data = dataSample(), aes_string(x = xInput , y = yInput, col = "Diagnosis")) + 
        geom_point() +
        geom_smooth() +
        #geom_line(size = 0.5) +
        #geom_hline(aes(yintercept = as.numeric(x)), linetype = 'dashed', color = 'red')+
        #geom_vline(aes(xintercept = as.numeric(y)), linetype = 'dashed', color = 'red')+
        labs(title = paste(xInput, "and", yInput),
             x = xInput, y = yInput)+
        theme_gdocs()

      ggplotly(p)
      
      print(p)

    })
    
    output$histogram <- renderPlotly({
      
      #sampleCancer$Diagnosis <- as.numeric(as.character(sampleCancer$Diagnosis))
      
      #corrplot(cor(sampleCancer), order = "hclust")
      
      p <- ggplot(sampleCancer, aes_string(x = input$x)) +
        geom_histogram(fill = "#69b3a2")
      
      ggplotly(p)
      
      print(p)

    })
    
    output$bar <- renderPlotly({
      #p <- ggplot(sampleCancer, aes_string(x = input$x, y = "Diagnosis", col = "Diagnosis")) +
        #geom_bar(stat = "identity", width = 0.5, aes_string(fill = "Diagnosis")) +
        #coord_flip()
      
      ggplot(sampleCancer, aes_string(x = input$x, fill = "Diagnosis")) +
        geom_bar(stat = "count", position = "stack", show.legend = TRUE)
    })
    
    output$lollipop <- renderPlotly({
      
      ggplot(sampleCancer, aes_string(x = input$x, y = input$y, col = "Diagnosis")) +
        geom_point() +
        geom_segment(aes_string(x = input$x, xend = input$x, y =0, yend = input$y)) +
        coord_flip()
      
      
    })
    
    
    
    
    output$boxplot <- renderPlotly({
      #p <- ggplot(cancerDataset, aes_string(x = "Diagnosis", y = "Texture_Mean", color = "Texture_Mean")) +
        #geom_boxplot() +
        #geom_jitter() +
        #ggtitle("Diagnosis based on Texture_Mean")
      
      #ggplotly(p)
      #print(p)
      
      cancerDataset1 <- melt(cancerDataset[2:32], id = "Diagnosis")
      
      ggplot(cancerDataset1, aes(x = variable, y = value, color = Diagnosis)) +
        #geom_jitter(aes(color = Diagnosis)) +
        geom_boxplot()
      
      
      
     
      
    })
    
    #output$summary <- renderText(      summary(sampleCancer))

})
