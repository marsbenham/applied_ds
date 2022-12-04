#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)
library(markdown)
library(bslib)
library(mongolite)
library(DT)
library(methods)
library(shinydashboard)
library(pool)
library(uuid)
library(dplyr)
library(DBI)
library(tidyverse)
library(rsconnect)
library(reactable)
library(ggplot2)
library(data.table)
library(vtable)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version=4,bootswatch="lux"),
  navbarPage("Pima Indigenous Database Application",
        tabPanel('Home',
                 h5('Pima Indigenous Diabetes Database Application and Exploration', align="center"),
                 h6("Mariah Benham, Applied Data Science DSCI-D590, Fall 2022", align="center"),
                 br(),
                 'The data has 768 cases and 9 columns, it was also found that the data contained no missing values and all predictor variables contained outliers. ', align='center',
                 br(),
                 br(),
                 fluidRow(
                   DT::dataTableOutput("table", width = "100%")
                 ),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 'Data sourced from UCI (University of California Irvine) Machine Learning Kaggle Account. ', align='center',
                 br(),
                 ),
        tabPanel('Outcome Comparisons',
                 'On this tab, you can select a predictor variable from the drop-down menu to view summary statistics and distribution of the variable based on the "Outcome" variable.',
                 br(),
                 br(),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput('PredictorVariable',
                                 "Select a predictor variable:",
                                 c('Number of pregnancies',
                                   'Glucose', 'Blood Pressure', 'Skin Thickness',
                                   'Insulin', 'BMI','Diabetes Pedigree Function', 'Age'))),
                   mainPanel(
                     br(),
                     uiOutput('hist'),
                     br(),
                     DT::dataTableOutput('table2'),
                     br(),
                     br(),
                     ('The "0" represents cases where the person does not have a formal diabetes diagnosis, the "1" represents cases where the person has been diagnosed with diabetes.')
                
                   )
                 )),
        tabPanel('Factor Analysis',
                 h4('Correlation Matrix'),
                 ('The first step of data analysis was to generate a correlation matrix, to determine the presence and strength of relationships between variables. '),
                 ('Only statistically significant correlations at the 0.01 level will be displayed in this plot.'),
                 plotOutput('corplot', width="100%", height='800px'),
                 br(),
                 br(),
                 h4('VIF Assessment'),
                 ('Next we will conduct a VIF test to determine if the assumption of multicollinearity is violated. '),
                 verbatimTextOutput('vif'),
                 ('VIF values are considered high when they are greater than 5 or 10, so our dataset does not violate this assumption.'),
                 br(),
                 br(),
                 h4('Kaiser-Meyer Olkin Test'),
                 ('The KMO test will allow us to determine if the dataset is suitable for Factor Analysis.'),
                 verbatimTextOutput('kmo'),
                 ('The overall MSA is > 0.5 which means we can proceed with Factor Analysis of the data.'),
                 br(),
                 br(),
                 h4('Finding the Number of Factors'),
                 ('To determine the number of factors for factor analysis, we created a scree plot of the eigenvalues.'),
                 plotOutput('screeplot', width='100%', height='500px'),
                 ('Based on the plot, the number of factors to use in this analysis will be 3.'),
                 br(),
                 br(),
                 h4('Factor Analysis Function'),
                 verbatimTextOutput('fa'),
                 ('The factanal() function indicates that using 3 factors is sufficient and statistically significant. It also shows that 46.7% of the variance in the dataset can be attributed to the 3 factors.'),
                 br(),
                 br(),
                 h4('Factor Analysis Diagram'),
                 ('The below image demonstrates how the individual predictor variables are compiled into principal axises (PAs).'),
                 br(),
                 imageOutput('fa_diagram')
                 ),
        tabPanel('Regression & Prediction Accuracy',
                 br(),
                 'Now using the data constructed during factor analysis, we can create a regression model.',
                 br(),
                 h4('Regression Data'),
                 br(),
                 DT::dataTableOutput('reg_data'),
                 br(),
                 h4('Building the Regression Model'),
                 br(),
                 'From there, the data will be split into training and testing data for the logistic regression model.',
                 br(),
                 verbatimTextOutput('log_model'),
                 br(),
                 h4('Regression Plots of Principal Axises'),
                 br(),
                 ('The regression models were then plotted using the PAs generated during Factor Analysis. '),
                 br(),
                 'The first regression model plots the PA that contains the Insulin and Glucose predictor variables.',
                 br(),
                 plotOutput('pa1'),
                 br(),
                 'The second regression model plots the PA that contains the Age and Number of Pregnancies predictor variables.',
                 br(),
                 plotOutput('pa2'),
                 br(),
                 'The third regression model plots the PA that contains the BMI, Skin Thickness, and Blood Pressure predictor variables.',
                 br(),
                 plotOutput('pa3'),
                 br(),
                 br(),
                 h6("The Model's Predictions"),
                 br(),
                 "Here we can compare the given 'Outcome' variable from the dataset and the 'Outcome' predicted by the regression model.",
                 br(),
                 DT::dataTableOutput('predict'),
                 br(),
                 h4("Accuracy of the Model"),
                 br(),
                 verbatimTextOutput('acc'),
                 br(),
                 'The model, as shown above, is 74% accurate',
                 br(),
                 h4('AUC-ROC Plot'),
                 br(),
                 'The AUC-ROC plot helps visualize the performance of the logistic regression model. AUC values range from 0-1, the higher the value the better the performance of the model.',
                 br(),
                 plotOutput('roc'),
                 br(),
                 'The AUC value is extremely close to the previous accuracy value of 74%, indicating the model is performing fairly well.',
                 br(),
                 br())
        )

        # Show a plot of the generated distribution
       
    )



# Define server logic required to draw a histogram
server <- function(input, output) {
  #home page output  
  output$table = DT::renderDataTable(data)
    
    #summary stats tab
    #histograms of input
    
    output$hist <- renderUI({
       
        if(input$PredictorVariable=='Number of pregnancies'){
          
          output$hist1 <- renderPlot({Pregnancies_plot})
          plotOutput('hist1')
          
        }
       else if(input$PredictorVariable=='Glucose'){
         
         output$hist2 <- renderPlot({Glucose_plot})
         plotOutput('hist2')
       }
      
      else if(input$PredictorVariable=='Blood Pressure'){
        
        output$hist3 <- renderPlot({BP_plot})
        plotOutput('hist3')
      }
      
      else if(input$PredictorVariable=='BMI'){
        
        output$hist6 <- renderPlot({BMI_plot})
        plotOutput('hist6')
      }
      
      else if(input$PredictorVariable=='Skin Thickness'){
        
        output$hist4 <- renderPlot({ST_plot})
        plotOutput('hist4')
      }
     
      else if(input$PredictorVariable=='Insulin'){
        
        output$hist5 <- renderPlot({ST_plot})
        plotOutput('hist5')
      }
      
      else if(input$PredictorVariable=='Diabetes Pedigree Function'){
        
        output$hist7 <- renderPlot({DPF_plot})
        plotOutput('hist7')
      }
      
      else if(input$PredictorVariable=='Age'){
        
        output$hist8 <- renderPlot({Age_plot})
        plotOutput('hist8')
      }
      
    })
    
  
    #summary statistics table code
                                  
    output$table2 <- DT::renderDataTable(
      if(input$PredictorVariable=='Number of pregnancies'){
        DT::datatable(pg_table, options=list(paging=FALSE,searching=FALSE))}
      
      else if(input$PredictorVariable=='Glucose'){
        DT::datatable(g_table, options=list(paging=FALSE,searching=FALSE))}
      
      else if(input$PredictorVariable=='Blood Pressure'){
        DT::datatable(bp_table, options=list(paging=FALSE,searching=FALSE))}
      
      else if(input$PredictorVariable=='BMI'){
        DT::datatable(bmi_table, options=list(paging=FALSE,searching=FALSE))}
      
      else if(input$PredictorVariable=='Skin Thickness'){
        DT::datatable(st_table, options=list(paging=FALSE,searching=FALSE))}
      
      else if(input$PredictorVariable=='Insulin'){
        DT::datatable(in_table, options=list(paging=FALSE,searching=FALSE))}
      
      else if(input$PredictorVariable=='Diabetes Pedigree Function'){
        DT::datatable(dpf_table, options=list(paging=FALSE,searching=FALSE))}
      
      else if (input$PredictorVariable=='Age'){
        DT::datatable(age_table, options=list(paging=FALSE,searching=FALSE))}
      
    )
    
    #Data Analysis Output Tab

    output$corplot <- renderPlot({
      
      corrplot(coeff$estimate, type="upper", p.mat=coeff$p.value, 
               order="hclust", sig.level=0.01,insig = "blank")
      
    })
    
    output$vif <- renderPrint({
      
      reg_model <- lm(Outcome~., data=data)
      
      vif(reg_model)
      })
    
    output$kmo <- renderPrint({
      
      KMO(r=data_matrix) 
      })
    
    
    output$screeplot <- renderPlot({
      
      scree <- data.frame(factor,ev)
      
      plot(scree, main='Scree Plot')
      lines(scree)
      abline(h=1, col='red')})
    
    
    output$fa <- renderPrint({
      
      factanal(fa_data,nfactors,scores=c('regression'),rotation='varimax')
    })
    
    #not working
    output$fa_diagram <- renderImage({
     list(src='~/Desktop/applied data science/000028.png', height='400px', width='600px',alt='Oops') 
     }, deleteFile = FALSE)
    
    # Regression Tab outputs start
    
    output$reg_data <- DT::renderDataTable(reg_data)
    
    output$log_model <- renderPrint({summary(log_model)})
    
    output$pa1 <- renderPlot({pa1})
    
    output$pa2 <- renderPlot({pa2})
    
    output$pa3 <- renderPlot({pa3})
    
    output$predict <- DT::renderDataTable(test[c(1,5)])
    
    output$acc <- renderPrint({acc})
    
    output$roc <- renderPlot({rocpred <- prediction(predict_reg,test$Outcome)
    rocper <- performance(rocpred, measure='tpr',x.measure='fpr')
    
    auc <- performance(rocpred, measure='auc')
    auc <- auc@y.values[[1]]
    
    plot(rocper)
    plot(rocper, colorize=TRUE,print.cutoffs.at=seq(0.1, by=0.1), main='ROC Curve')
    abline(a=0,b=1)
    
    auc <- round(auc,4)
    legend(0.6,0.4,auc,title='AUC',cex=1)})
}
    

# Run the application 
shinyApp(ui = ui, server = server)

  

