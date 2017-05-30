library(shiny)
library(shinydashboard)
library(ggplot2)
library(fpc)
library(e1071)

example_text <- "Statistics is the study of the collection, analysis, interpretation, presentation, and organization of data.
When census data cannot be collected, statisticians collect data by developing specific experiment designs and survey samples. Representative sampling assures that inferences and conclusions can safely extend from the sample to the population as a whole. An experimental study involves taking measurements of the system under study, manipulating the system, and then taking additional measurements using the same procedure to determine if the manipulation has modified the values of the measurements. In contrast, an observational study does not involve experimental manipulation.
Two main statistical methodologies are used in data analysis: descriptive statistics, which summarizes data from a sample using indexes such as the mean or standard deviation, and inferential statistics, which draws conclusions from data that are subject to random variation (e.g., observational errors, sampling variation).[2] Descriptive statistics are most often concerned with two sets of properties of a distribution (sample or population): central tendency (or location) seeks to characterize the distribution's central or typical value, while dispersion (or variability) characterizes the extent to which members of the distribution depart from its center and each other. Inferences on mathematical statistics are made under the framework of probability theory, which deals with the analysis of random phenomena.
A standard statistical procedure involves the test of the relationship between two statistical data sets, or a data set and a synthetic data drawn from idealized model. An hypothesis is proposed for the statistical relationship between the two data sets, and this is compared as an alternative to an idealized null hypothesis of no relationship between two data sets. Rejecting or disproving the null hypothesis is done using statistical tests that quantify the sense in which the null can be proven false, given the data that are used in the test. Working from a null hypothesis, two basic forms of error are recognized: Type I errors (null hypothesis is falsely rejected giving a \"false positive\") and Type II errors (null hypothesis fails to be rejected and an actual difference between populations is missed giving a \"false negative\").
Multiple problems have come to be associated with this framework: ranging from obtaining a sufficient sample size to specifying an adequate null hypothesis.[citation needed]
Measurement processes that generate statistical data are also subject to error. Many of these errors are classified as random (noise) or systematic (bias), but other important types of errors (e.g., blunder, such as when an analyst reports incorrect units) can also be important. The presence of missing data and/or censoring may result in biased estimates and specific techniques have been developed to address these problems.
Statistics can be said to have begun in ancient civilization, going back at least to the 5th century BC, but it was not until the 18th century that it started to draw more heavily from calculus and probability theory. Statistics continues to be an area of active research, for example on the problem of how to analyze Big data."


shinyServer(function(input,output,session){

#WORD COUNT
  # count total characters
  output$char_count <- renderText(paste("Total characters:  ",nchar(input$text),sep=""))
  
  # count total words
  text2words <- function(text){
    text_raw <- str_replace_all(text,"[\\W]","\ ")
    words <- str_split(text_raw," ")[[1]]
    words <- words[words != ""]
    return(words)
  }
  output$total_count <- renderText(paste("Total words:  ",length(text2words(input$text)),sep=""))
  
  # barplot of high frequency words
  sep_count <- function(words){
    name_vec <- c()
    count_vec <- c()
    n <- 0
    for(word in words){
      if(sum(str_count(name_vec,word)) == 0){
        n = n + 1
        name_vec[n] <- word
        count_vec[n] <- 1
      }
      else{
        index <- which(name_vec == word)
        count_vec[index] = count_vec[index] + 1
      }
    }
    res <- data.frame(name=name_vec,count=count_vec)
    res <- res[order(res$count,decreasing = T),]
    res$name <- factor(res$name,levels=res[order(res$count),1])
    return(res)
  }
  output$seperate_bar <- renderPlot({
    words <- text2words(input$text)
    if(length(words) > 0){
      res <- head(sep_count(words),input$topn)
      ggplot(data=res,aes(x=name,y=count))+geom_bar(stat="identity")+coord_flip()+ggtitle("Words with the heighest frequency")
    }
  })
  
  # example
  observeEvent(input$example,{
    updateTextInput(session,"text",value=example_text)
  })
  
  # clear text
  observeEvent(input$clear,{
    updateTextInput(session,"text",value="")
  })  
  
#ANALYSIS
  
  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
    }
  })
  
  myData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      read.table(inFile()$datapath, sep = ',', header = TRUE,  stringsAsFactors = FALSE)
    }
  })
  datasetInput <- reactive({
    switch(input$dataset,
           "cars" = mtcars,
           "longley" = longley,
           "rock" = rock,
           "pressure" = pressure, 
           "Uploaded Data" = myData())
  })

  output$table <- DT::renderDataTable({
    DT::datatable(datasetInput())
  })
   output$sum<-renderTable({
    if(is.null(datasetInput())){return()}
    summary(datasetInput())
  })
   #kmeans
  observe({
    updateSelectInput(
      session,
      "xcol",
      choices=names(datasetInput()))
    
  })
  observe({
    updateSelectInput(
      session,
      "ycol",
      choices=names(datasetInput()))
    
  })
  selectedData <- reactive({
    datasetInput()[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
        if (is.null(inFile())) {
      h5("Powered by", tags$img(src='RStudio-Ball.png', height=200, width=200))
    } else {
    plot(selectedData(),col=clusters()$cluster)
    points(clusters()$centers,col=1:3, pch = 4, cex = 4)
    }
  })
  #Fuzzy cmeans
  observe({
    updateSelectInput(
      session,
      "fc_xcol",
      choices=names(datasetInput()))
    
  })
  observe({
    updateSelectInput(
      session,
      "fc_ycol",
      choices=names(datasetInput()))
    
  })
  fc_selectedData <- reactive({
    datasetInput()[, c(input$fc_xcol, input$fc_ycol)]
  })
  
  fc_clusters <- reactive({
    cmeans(fc_selectedData(),input$fc_clusters,20,verbose =TRUE, dist = "euclidean" , method = "cmeans" ,2)
  })
  
  output$fc_plot <- renderPlot({
    if (is.null(inFile())) {
      h5("Powered by", tags$img(src='RStudio-Ball.png', height=200, width=200))
    } else {
      plot(fc_selectedData(),col=fc_clusters()$cluster)
      points(fc_clusters()$centers,col=1:3, pch = 4, cex = 4)
    }
  })
  
  #classification
  #svm
  output$svm_dv = renderUI({
    selectInput('svm_dv', h5('Class Label'), choices = names(datasetInput()))
  })
  
  output$svm_iv = renderUI({
    selectInput('svm_iv', h5('Attributes'), choices = names(datasetInput()))
  })
  
  svmFormula <- reactive({
    as.formula(paste(input$svm_dv, '~', input$svm_iv))
  })
  
  svm_model <- reactive({
    svm(svmFormula(), data = datasetInput(),kernel = input$kernel)
  })

  prediction <- reactive({
    predict(svm_model(), data = datasetInput())
  })  
  
  output$svm_plot <- renderPlot({
    plot(datasetInput()[,input$svm_dv],datasetInput()[,input$svm_iv] , pch = 16,col = "black", cex = 1)
    plot(datasetInput()[,input$svm_dv],pch = 16)
    points(datasetInput()[,input$svm_dv], prediction() , col = "red", pch=4)
  })
  
  output$svm_model <- renderPrint({
    summary(svm_model())
  })
  
  output$residual_svm <- renderPrint({
    svm_model()$residuals
  })
  error <- reactive({
    svm_model()$residuals
  })
  output$error_svm <- renderPrint({
    rmse(error())
  })
  #regression
  # dependent variable
  output$dv = renderUI({
    selectInput('dv', h5('Dependent Variable'), choices = names(datasetInput()))
  })
  
  # independent variable
  output$iv = renderUI({
    selectInput('iv', h5('Independent Variable'), choices = names(datasetInput()))
  })
  # regression formula
  regFormula <- reactive({
    as.formula(paste(input$dv, '~', input$iv))
  })
  # bivariate model
  model <- reactive({
    lm(regFormula(), data = datasetInput())
  }) 
  
  # histograms   
  output$distPlot_dv <- renderPlot({
    x    <- datasetInput()[,input$dv]  
    bins <- seq(min(x), max(x), length.out = input$bins_dv + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = 'Dependent Variable', xlab = input$dv)
  })
  
  
  output$distPlot_iv <- renderPlot({
    x    <- datasetInput()[,input$iv]  
    bins <- seq(min(x), max(x), length.out = input$bins_iv + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = 'Independent Variable', xlab = input$iv)
  })
  
  # scatter plot 
  output$scatter <- renderPlot({
    plot(datasetInput()[,input$iv], datasetInput()[,input$dv],
         xlab = input$iv, ylab = input$dv,  main = "Scatter Plot of Independent and Dependent Variables", pch = 16, 
         col = "black", cex = 1) 
    
    abline(lm(datasetInput()[,input$dv]~datasetInput()[,input$iv]), col="grey", lwd = 2) 
  })
  
  # correlation matrix
  output$corr <- renderGvis({
    d <- datasetInput()[,sapply(datasetInput(),is.integer)|sapply(datasetInput(),is.numeric)] 
    cor <- as.data.frame(round(cor(d), 2))
    cor <- cbind(Variables = rownames(cor), cor)
    gvisTable(cor) 
  })
  # bivariate model
  output$model <- renderPrint({
    summary(model())
  })
  
  # residuals
  output$residuals_hist <- renderPlot({
    hist(model()$residuals, main = paste(input$dv, '~', input$iv), xlab = 'Residuals') 
  })
  
  output$residuals_scatter <- renderPlot({
    plot(model()$residuals ~ datasetInput()[,input$iv], xlab = input$iv, ylab = 'Residuals')
    abline(h = 0, lty = 3) 
  })
  
  output$residuals_qqline <- renderPlot({
    qqnorm(model()$residuals)
    qqline(model()$residuals) 
  })
  
 
  # download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    })
  
})