
#install.packages("neuralnet")
#install.packages("e1071")


#library(neuralnet)
#library(e1071)
#library(glm2)


#library(ggplot2)
library(shiny)
library(forecast)
library(e1071)
library(kernlab)
library(deepnet)
library(RSNNS)
library(neuralnet)
library(MASS)
library(Hmisc)
library(eegkit)
library(eegAnalysis)
library(shinyRGL)
#require(SMA)

shinyServer(
  
  function(input,output)
  {
    data <- reactive({
      file1 <- input$file
  
      if(is.null(file1))
      {return()}
      read.table(file=file1$datapath, sep=input$sep, header=input$header, stringsAsFactors=input$stringsasfactors) 
    })
    
    data2 <- reactive({
      file2 <- input$filealpha
      
      if(is.null(file2))
      {return()}
      read.table(file=file2$datapath, sep=input$sep, header=input$header, stringsAsFactors=input$stringsasfactors)
    })
    
    data3 <- reactive({
      file3 <- input$filebeta
      
      if(is.null(file3))
      {return()}
      read.table(file=file3$datapath, sep=input$sep, header=input$header, stringsAsFactors=input$stringsasfactors)
    })
    
    data4 <- reactive({
      file4 <- input$filegamma
      
      if(is.null(file4))
      {return()}
      read.table(file=file4$datapath, sep=input$sep, header=input$header, stringsAsFactors=input$stringsasfactors)
    })
    
    data5 <- reactive({
      file5 <- input$filetheta
      
      if(is.null(file5))
      {return()}
      read.table(file=file5$datapath, sep=input$sep, header=input$header, stringsAsFactors=input$stringsasfactors)
    })
    
    data6 <- reactive({
      file6 <- input$filetest
      
      if(is.null(file6))
      {return()}
      read.table(file=file6$datapath, sep=input$sep, header=input$header, stringsAsFactors=input$stringsasfactors)
    })
    
    
    output$filedf <- renderText({
      "                             
      
      ------------------------------------------------------------------------------------------------------------------
      
      In this project we are trying to classify the workload on various subjects using various non-linear deep learning 
      algorithms. We are using the Electroencephalographical Recordings (EEG) of both the subjects and then after feature 
      extraction applying various non-linear classifiers to analyze the features.
      
      We are implementing 5 deep learning algorithms - 
      
      1. Types of Artifical Neural Networks (+RBackprop, -RBackprop, etc)
      2. Support Vector Machines (SVMs)
      3. Stacked Autoencoders (SAEs)
      4. Radial Basic Function (RBFs)
      5. Linear Discriminant Analysis (LDAs) 
      
      The accuracy of each model is calculated and the final classification is produced in terms of the 3 classes-
      Base Line (BL), Low Work Load (LWL) and High Work Load (HWL).  
      The models designed, take the input as 14/64 channel data recordings and predict the outcome in form of these 3
      classes. 
      
      Furthermore, we are trying to analyze the affect of each individual feature of the signals (alpha, beta, etc)
      on the final result and find out the correlation between these features.
      The correlation is depicted using Pearson's correlation coefficient. 
      
      The models thus trained with the testing data will be capable of classifying the workload on any subject based 
      on the EEG recordings and predict whether the user is having high, low or no workload. 
      
      -------------------------------------------------------------------------------------------------------------------"
    })
    
    
    
    ####################################################################################################################################################
    ################################################# 14 channel functions #############################################################################
    ####################################################################################################################################################
    
    output$table <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table1 <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table2 <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table3 <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table4 <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table5 <- renderTable({
      if(is.null(data())){return()}
      data2() 
    })
    #####################################################################################################################################################
    output$ann <- renderTable({
      file2 <- data()
      file2 <- data.frame(file2)
      
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      
      net <- neuralnet(BL+LWL+HWL~AF3+F7+F3+FC5+T7+P7+O1+O2+P8+T8+FC6+F4+F8+AF4, file2, hidden=8, rep=10, algorithm = "rprop+", err.fct="ce", linear.output=FALSE)
      
      j <- compute(net, c)$net.result
      
      Base_Load <- j[1,1]
      Low_Work_Load <- j[1,2]
      High_Work_Load <- j[1,3]
      
      result <- data.frame(Base_Load, Low_Work_Load, High_Work_Load)
      
    })
    
    
    ########################################################################################################################################
    
    
    output$plotann <- renderPlot({
      file2 <- data()
      file2 <- data.frame(file2)
      
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      
      net <- neuralnet(BL+LWL+HWL~AF3+F7+F3+FC5+T7+P7+O1+O2+P8+T8+FC6+F4+F8+AF4, file2, hidden=8, rep=10, algorithm = "rprop+",err.fct="ce", linear.output=FALSE)
      
      plot(net, rep = "best")
      
    })
    
    #############################################################################################################################################
    
    
    
    output$svm <- renderTable({
      file2 <- data()
      file2 <- data.frame(file2)
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      AF3  <- as.numeric(input$c1)
      F7 <- as.numeric(input$c2)
      F3 <- as.numeric(input$c3)
      FC5 <- as.numeric(input$c4)
      T7 <- as.numeric(input$c5)
      P7 <- as.numeric(input$c6)
      O1 <- as.numeric(input$c7)
      O2 <- as.numeric(input$c8)
      P8 <- as.numeric(input$c9)
      T8 <- as.numeric(input$c10)
      FC6 <- as.numeric(input$c11)
      F4 <- as.numeric(input$c12)
      F8 <- as.numeric(input$c13)
      AF4 <- as.numeric(input$c14)
      c <- data.frame(AF3,F7,F3,FC5,T7,P7,O1,O2,P8,T8,FC6,F4,F8,AF4)
      
      k <- y$BL*2^2+y$LWL*2^1+y$HWL*2^0
      k <- data.frame(k)
      
      for (i in 1:nrow(x))
      {
        if(k[i,1] == 4)
        {k[i,1] <- "Base_Line"}
        else if(k[i,1] == 2)
        {k[i,1] <- "Low_Work_Load"}
        else if(k[i,1] == 1)
        {k[i,1] <- "High_Work_Load"}
      }
      
      z <- subset(file2, select = -c(BL,LWL,HWL))
      e <- data.frame(z,k)
      e$k <- as.factor(e$k)
      
      svm_model <- svm(k~., data = e)
      
      svm_tune <- tune(svm, train.x=x, train.y=e$k, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
      
      svm_model_after_tune <- svm(k ~ ., data=e, kernel="radial", cost=svm_tune$best.parameters[1,1], gamma=svm_tune$best.parameters[1,2])
      
      pred <- predict(svm_model_after_tune,c)
      pred <- data.frame(pred)
      
    })
    
    #cannot plot svm as svm plot only takes 2 col of input data and we have 14 cols
    #output$plotsvm <- renderPlot({
    
    # plot(svm_model_after_tune, data = e)
    
    #})
    
    ###################################################################################################################################
    output$sda <- renderTable({
      file2 <- data()
      file2 <- data.frame(file2)
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      
      
      norm_x <- (x-min(x))/(max(x)-min(x))
      norm_y <- (y-min(y))/(max(y)-min(y))
      
      mat_x <- as.matrix(norm_x)
      mat_y <- as.matrix(norm_y)
      dnn <- sae.dnn.train(mat_x, mat_y, hidden = c(8))
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      #print(c)
      norm_c <- (c - min(c))/(max(c)-min(c))
      mat_c <- as.matrix(norm_c)
      #print(mat_c)
      predict_c <- nn.predict(dnn, mat_c)
      predict_c <- data.frame(predict_c)
      denorm_c <- (predict_c*(max(c)-min(c))+min(c))
      denorm_c <- data.frame(denorm_c)
      
      temp <- denorm_c[1,1]
      
      if(temp>denorm_c[1,2])
      {
        if(temp > denorm_c[1,3])
        {
          final_c <- "Base Line"
        }else if(temp < denorm_c[1,3])
        {
          final_c <- "High_Work_Load"
        }
      }else 
        {
          final_c <- "Low_Work_Load"
        }
        
      
      Work_Load <- as.data.frame(final_c)
      Work_Load <- data.frame(Work_Load)
      
      
    })
    
    
    #####################################################################################################################################
    output$rbf <- renderTable({
      file2 <- data()
      file2 <- data.frame(file2)
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      
      r <- rbf(x, y, size = c(8), maxit = 100, initFunc = "RBF_Weights", learnFuncParams = c(1e-05, 0, 1e-05, 0.1, 0.8), updateFunc = "Topological_Order", updateFuncParams = c(0),shufflePatterns = TRUE, linOut = TRUE)
      rc <- predict(r, c)
      temp <- rc[1,1]
      if(temp>rc[1,2])
      {
        if(temp > rc[1,3])
        {
          final_c <- "Base Line"
        }else if(temp < rc[1,3])
        {
          final_c <- "High_Work_Load"
        }
      }else 
        {
          final_c <- "Low_Work_Load"
        }
        
      
      
      Work_Load <- as.data.frame(final_c)
      Work_Load <- data.frame(Work_Load)
      
      
    })
    #####################################################################################################################################
    output$rbfplot <- renderPlot({
      file2 <- data()
      file2 <- data.frame(file2)
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      
      r <- rbf(x, y, size = c(8), maxit = 100, initFunc = "RBF_Weights", learnFuncParams = c(1e-05, 0, 1e-05, 0.1, 0.8), updateFunc = "Topological_Order", updateFuncParams = c(0),shufflePatterns = TRUE, linOut = TRUE)
      plotIterativeError(r)
      
      
    })
   ###################################################################################################################################### 
    
    output$lda <- renderTable({
      
      f2 <- data()
      f2 <- data.frame(f2)
      x <- subset(f2, select = -c(BL, LWL, HWL))
      
      BL <- f2$BL
      LWL <- f2$LWL
      HWL <- f2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      AF3 <- as.numeric(input$c1)
      F7 <- as.numeric(input$c2)
      F3 <- as.numeric(input$c3)
      FC5 <- as.numeric(input$c4)
      T7 <- as.numeric(input$c5)
      P7 <- as.numeric(input$c6)
      O1 <- as.numeric(input$c7)
      O2 <- as.numeric(input$c8)
      P8 <- as.numeric(input$c9)
      T8 <- as.numeric(input$c10)
      FC6 <- as.numeric(input$c11)
      F4 <- as.numeric(input$c12)
      F8 <- as.numeric(input$c13)
      AF4 <- as.numeric(input$c14)
      c <- data.frame(AF3,F7,F3,FC5,T7,P7,O1,O2,P8,T8,FC6,F4,F8,AF4)
      
      k <- y$BL*2^2+y$LWL*2^1+y$HWL*2^0
      k <- data.frame(k)
      for (i in 1:nrow(x))
      {
        if(k[i,1] == 4)
        {k[i,1] <- "Base_Line"}
        else if(k[i,1] == 2)
        {k[i,1] <- "Low_Work_Load"}
        else if(k[i,1] == 1)
        {k[i,1] <- "High_Work_Load"}
      }
      print(k)
      z <- subset(f2, select = -c(BL,LWL,HWL))
      e <- data.frame(z,k)
      e$k <- as.factor(e$k)
      
      fit <- lda(e$k~., data=e , na.action="na.omit", CV=FALSE)
      print(fit)
      
      final <- predict(fit, c)$class
      print(final)
      final <- data.frame(final)
      print(final)
    })
    ###############################################################################################################
    
    output$plotlda <- renderPlot({
      f2 <- data()
      f2 <- data.frame(f2)
      x <- subset(f2, select = -c(BL, LWL, HWL))
      
      BL <- f2$BL
      LWL <- f2$LWL
      HWL <- f2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      AF3  <- as.numeric(input$c1)
      F7 <- as.numeric(input$c2)
      F3 <- as.numeric(input$c3)
      FC5 <- as.numeric(input$c4)
      T7 <- as.numeric(input$c5)
      P7 <- as.numeric(input$c6)
      O1 <- as.numeric(input$c7)
      O2 <- as.numeric(input$c8)
      P8 <- as.numeric(input$c9)
      T8 <- as.numeric(input$c10)
      FC6 <- as.numeric(input$c11)
      F4 <- as.numeric(input$c12)
      F8 <- as.numeric(input$c13)
      AF4 <- as.numeric(input$c14)
      c <- data.frame(AF3,F7,F3,FC5,T7,P7,O1,O2,P8,T8,FC6,F4,F8,AF4)
      
      k <- y$BL*2^2+y$LWL*2^1+y$HWL*2^0
      k <- data.frame(k)
      for (i in 1:nrow(x))
      {
        if(k[i,1] == 4)
        {k[i,1] <- "Base_Line"}
        else if(k[i,1] == 2)
        {k[i,1] <- "Low_Work_Load"}
        else if(k[i,1] == 1)
        {k[i,1] <- "High_Work_Load"}
      }
      
      z <- subset(f2, select = -c(BL,LWL,HWL))
      e <- data.frame(z,k)
      e$k <- as.factor(e$k)
      
      fit <- lda(e$k~., data=e , na.action="na.omit", CV=FALSE)
    
      final <- predict(fit, c)$class
      
      final <- data.frame(final)
      
      par(mfrow = c(2,1))
      plot(fit, dimen=1, type="both", cex = 1.5)
      
    })
    
    ##################################################################################################################
    output$cr1 <- renderTable({
      f1 <- data2()
      f2 <- data3()
      f3 <- data4()
      f4 <- data5()
      
      alpha <- data.frame(f1)
      beta <- data.frame(f2)
      gamma <- data.frame(f3)
      theta <- data.frame(f4)
      
      f <- data.frame(f1, f2, f3, f4)
      
      tempo1 <- summary(aov(alpha$BL~ beta$BL + gamma$BL + theta$BL))[[1]]
      #tempo2 <- summary(aov(alpha$LWL ~ beta$LWL + gamma$LWL + theta$LWL))[[1]]
      #tempo3 <- summary(aov(alpha$HWL ~ beta$HWL + gamma$HWL + theta$HWL))[[1]]
      
      #summary <- data.frame(tempo1, tempo2, tempo3)
      
      
    })
    
    output$cr2 <- renderTable({
      f1 <- data2()
      f2 <- data3()
      f3 <- data4()
      f4 <- data5()
      
      alpha <- data.frame(f1)
      beta <- data.frame(f2)
      gamma <- data.frame(f3)
      theta <- data.frame(f4)
      
      f <- data.frame(f1, f2, f3, f4)
      
      #tempo1 <- summary(aov(alpha$BL~ beta$BL + gamma$BL + theta$BL))[[1]]
      tempo2 <- summary(aov(alpha$LWL ~ beta$LWL + gamma$LWL + theta$LWL))[[1]]
      #tempo3 <- summary(aov(alpha$HWL ~ beta$HWL + gamma$HWL + theta$HWL))[[1]]
      
      #summary <- data.frame(tempo1, tempo2, tempo3)
      
      
    })
    
    output$cr3 <- renderTable({
      f1 <- data2()
      f2 <- data3()
      f3 <- data4()
      f4 <- data5()
      
      alpha <- data.frame(f1)
      beta <- data.frame(f2)
      gamma <- data.frame(f3)
      theta <- data.frame(f4)
      
      f <- data.frame(f1, f2, f3, f4)
      
      #tempo1 <- summary(aov(alpha$BL~ beta$BL + gamma$BL + theta$BL))[[1]]
      #tempo2 <- summary(aov(alpha$LWL ~ beta$LWL + gamma$LWL + theta$LWL))[[1]]
      tempo3 <- summary(aov(alpha$HWL ~ beta$HWL + gamma$HWL + theta$HWL))[[1]]
      
      #summary <- data.frame(tempo1, tempo2, tempo3)
      
      
    })
    
    output$crtext <- renderText({
      
      "
       These are the Summary Tables for each of the Work Load Classification.

       The 1st table shows the significance of alpha on beta, gamma and theta values for the BL(Base Line) Class.
       The 2nd table shows the significance of alpha on beta, gamma and theta values for the LWL(Low Workload) Class.
       The 3rd table shows the significance of alpha on beta, gamma and theta values for the HWL(High Workload) Class.
      
       We have made use of the ANOVA (Analysis of Varaiance) test to analyse the significance.
       
       "
    })
    
    output$cr2text <- renderText({
      
      "
      Below are the correlation values between each possible pair of (alpha, beta, gamma, theta) values for each class. 
      Here we have made use of Hmisc Package to calculate the correlation using PEARSON's method. 

      The below 3 tables display the correlation values for each possible pair in the following classes respectively- 
      
      1. Base Line (BL). 
      2. Low Work Load (LWL).
      3. High Work Load (HWL).
      
      "
    })
    
    
    output$crr1 <- renderTable({
      f1 <- data2()
      f2 <- data3()
      f3 <- data4()
      f4 <- data5()
      
      alpha <- data.frame(f1)
      beta <- data.frame(f2)
      gamma <- data.frame(f3)
      theta <- data.frame(f4)
      
      alpha_beta <- (rcorr(alpha$BL, beta$BL, type = "pearson"))$r[1,2]
      alpha_gamma <- (rcorr(alpha$BL, gamma$BL, type = "pearson"))$r[1,2]
      alpha_theta <- (rcorr(alpha$BL, theta$BL, type = "pearson"))$r[1,2]
      beta_gamma <- (rcorr(beta$BL, gamma$BL, type = "pearson"))$r[1,2]
      beta_theta <- (rcorr(beta$BL, theta$BL, type = "pearson"))$r[1,2]
      gamma_theta <- (rcorr(gamma$BL, theta$BL, type = "pearson"))$r[1,2]
      
      correlation1 <- data.frame(alpha_beta, alpha_gamma, alpha_theta, beta_gamma, beta_theta, gamma_theta)
    })
    
    output$crr2 <- renderTable({
      f1 <- data2()
      f2 <- data3()
      f3 <- data4()
      f4 <- data5()
      
      alpha <- data.frame(f1)
      beta <- data.frame(f2)
      gamma <- data.frame(f3)
      theta <- data.frame(f4)
      
      alpha_beta <- (rcorr(alpha$LWL, beta$LWL, type = "pearson"))$r[1,2]
      alpha_gamma <- (rcorr(alpha$LWL, gamma$LWL, type = "pearson"))$r[1,2]
      alpha_theta <- (rcorr(alpha$LWL, theta$LWL, type = "pearson"))$r[1,2]
      beta_gamma <- (rcorr(beta$LWL, gamma$LWL, type = "pearson"))$r[1,2]
      beta_theta <- (rcorr(beta$LWL, theta$LWL, type = "pearson"))$r[1,2]
      gamma_theta <- (rcorr(gamma$LWL, theta$LWL, type = "pearson"))$r[1,2]
      
      correlation1 <- data.frame(alpha_beta, alpha_gamma, alpha_theta, beta_gamma, beta_theta, gamma_theta)
    })
    
    output$crr3 <- renderTable({
      f1 <- data2()
      f2 <- data3()
      f3 <- data4()
      f4 <- data5()
      
      alpha <- data.frame(f1)
      beta <- data.frame(f2)
      gamma <- data.frame(f3)
      theta <- data.frame(f4)
      
      alpha_beta <- (rcorr(alpha$HWL, beta$HWL, type = "pearson"))$r[1,2]
      alpha_gamma <- (rcorr(alpha$HWL, gamma$HWL, type = "pearson"))$r[1,2]
      alpha_theta <- (rcorr(alpha$HWL, theta$HWL, type = "pearson"))$r[1,2]
      beta_gamma <- (rcorr(beta$HWL, gamma$HWL, type = "pearson"))$r[1,2]
      beta_theta <- (rcorr(beta$HWL, theta$HWL, type = "pearson"))$r[1,2]
      gamma_theta <- (rcorr(gamma$HWL, theta$HWL, type = "pearson"))$r[1,2]
      
      correlation1 <- data.frame(alpha_beta, alpha_gamma, alpha_theta, beta_gamma, beta_theta, gamma_theta)
    })
    
    ########################################################################################################################
    output$sample <- renderTable({
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      
    })
    
    output$sample1 <- renderTable({
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      
    })
    
    output$sample2 <- renderTable({
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      
    })
    
    output$sample3 <- renderTable({
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      
    })
    
    output$sample4 <- renderTable({
      c1 <- as.numeric(input$c1)
      c2 <- as.numeric(input$c2)
      c3 <- as.numeric(input$c3)
      c4 <- as.numeric(input$c4)
      c5 <- as.numeric(input$c5)
      c6 <- as.numeric(input$c6)
      c7 <- as.numeric(input$c7)
      c8 <- as.numeric(input$c8)
      c9 <- as.numeric(input$c9)
      c10 <- as.numeric(input$c10)
      c11 <- as.numeric(input$c11)
      c12 <- as.numeric(input$c12)
      c13 <- as.numeric(input$c13)
      c14 <- as.numeric(input$c14)
      c <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14)
      
    })
 
    ##########################################################################################################################################
    ################################################ 64 Channel functions ####################################################################
    ##########################################################################################################################################
    
    output$table64 <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table164 <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table264 <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table364 <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table464 <- renderTable({
      if(is.null(data())){return()}
      data()  
    })
    
    output$table564 <- renderTable({
      if(is.null(data())){return()}
      data2() 
    })
    
    ##################################################################################################################################
    
    output$sample64 <- renderTable({
      test <- data6()
      test <- data.frame(test)
    })
    
    output$sample164 <- renderTable({
      test <- data6()
      test <- data.frame(test)
    })
    
    output$sample264 <- renderTable({
      test <- data6()
      test <- data.frame(test)
    })
    
    output$sample364 <- renderTable({
      test <- data6()
      test <- data.frame(test)
    })
    
    output$sample464 <- renderTable({
      test <- data6()
      test <- data.frame(test)
    })
    
    ####################################################################################################################################
    
    output$ann64 <- renderTable({
      file2 <- data()
      file2 <- data.frame(file2)
      
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      c <- data.frame(data6())
      
      net <- neuralnet(BL+LWL+HWL~., file2, hidden=c(38,20), rep=50, algorithm = "rprop+", err.fct="ce", linear.output=FALSE)
      
      j <- compute(net, c)$net.result
      
      Base_Load <- j[1,1]
      Low_Work_Load <- j[1,2]
      High_Work_Load <- j[1,3]
      
      result <- data.frame(Base_Load, Low_Work_Load, High_Work_Load)
      
    })
    
    ########################################################################################################################################
    
    output$plotann64 <- renderPlot({
      file2 <- data()
      file2 <- data.frame(file2)
      
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      c <- data.frame(data6())
      
      net <- neuralnet(BL+LWL+HWL~., file2, hidden=c(38,20), rep=50, algorithm = "rprop+", err.fct="ce", linear.output=FALSE)
      
      plot(net, rep = "best")
    })
    
    
    #########################################################################################################################################
    
    output$svm64 <- renderTable({
      
      file2 <- data()
      file2 <- data.frame(file2)
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      
      c <- data.frame(data6())
      
      k <- y$BL*2^2+y$LWL*2^1+y$HWL*2^0
      k <- data.frame(k)
      
      for (i in 1:nrow(x))
      {
        if(k[i,1] == 4)
        {k[i,1] <- "Base_Line"}
        else if(k[i,1] == 2)
        {k[i,1] <- "Low_Work_Load"}
        else if(k[i,1] == 1)
        {k[i,1] <- "High_Work_Load"}
      }
      
      z <- subset(file2, select = -c(BL,LWL,HWL))
      e <- data.frame(z,k)
      e$k <- as.factor(e$k)
      
      svm_model <- svm(k~., data = e)
      
      svm_tune <- tune(svm, train.x=x, train.y=e$k, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
      
      svm_model_after_tune <- svm(k ~ ., data=e, kernel="radial", cost=svm_tune$best.parameters[1,1], gamma=svm_tune$best.parameters[1,2])
      
      pred <- predict(svm_model_after_tune,c)
      pred <- data.frame(pred)
      
    })
    
    ################################################################################################################################################
    
    output$sda64 <- renderTable({
      file2 <- data()
      file2 <- data.frame(file2)
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      
      
      norm_x <- (x-min(x))/(max(x)-min(x))
      norm_y <- (y-min(y))/(max(y)-min(y))
      
      mat_x <- as.matrix(norm_x)
      mat_y <- as.matrix(norm_y)
      dnn <- sae.dnn.train(mat_x, mat_y, hidden = c(8))
      
      c <- data.frame(data6())
      #print(c)
      norm_c <- (c - min(c))/(max(c)-min(c))
      mat_c <- as.matrix(norm_c)
      #print(mat_c)
      predict_c <- nn.predict(dnn, mat_c)
      predict_c <- data.frame(predict_c)
      denorm_c <- (predict_c*(max(c)-min(c))+min(c))
      denorm_c <- data.frame(denorm_c)
      
      temp <- denorm_c[1,1]
      
      if(temp>denorm_c[1,2])
      {
        if(temp > denorm_c[1,3])
        {
          final_c <- "Base Line"
        }else if(temp < denorm_c[1,3])
        {
          final_c <- "High_Work_Load"
        }
      }else 
      {
        final_c <- "Low_Work_Load"
      }
      
      
      Work_Load <- as.data.frame(final_c)
      Work_Load <- data.frame(Work_Load)
      
      
    })
    #################################################################################################################################################
    
    output$rbf64 <- renderTable({
      file2 <- data()
      file2 <- data.frame(file2)
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      c <- data.frame(data6())
      
      r <- rbf(x, y, size = c(38,20), maxit = 1000, initFunc = "RBF_Weights", learnFuncParams = c(1e-05, 0, 1e-05, 0.1, 0.8), updateFunc = "Topological_Order", updateFuncParams = c(0),shufflePatterns = TRUE, linOut = TRUE)
      rc <- predict(r, c)
      temp <- rc[1,1]
      if(temp>rc[1,2])
      {
        if(temp > rc[1,3])
        {
          final_c <- "Base Line"
        }else if(temp < rc[1,3])
        {
          final_c <- "High_Work_Load"
        }
      }else 
      {
        final_c <- "Low_Work_Load"
      }
      
      
      
      Work_Load <- as.data.frame(final_c)
      Work_Load <- data.frame(Work_Load)
      
      
    })
    #####################################################################################################################################
    output$rbfplot64 <- renderPlot({
      file2 <- data()
      file2 <- data.frame(file2)
      x <- subset(file2, select = -c(BL, LWL, HWL))
      
      BL <- file2$BL
      LWL <- file2$LWL
      HWL <- file2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      c <- data.frame(data6())
      
      r <- rbf(x, y, size = c(38,20), maxit = 1000, initFunc = "RBF_Weights", learnFuncParams = c(1e-05, 0, 1e-05, 0.1, 0.8), updateFunc = "Topological_Order", updateFuncParams = c(0),shufflePatterns = TRUE, linOut = TRUE)
      plotIterativeError(r)
      
      
    })
    ###################################################################################################################################### 
    
    output$lda64 <- renderTable({
      
      f2 <- data()
      f2 <- data.frame(f2)
      x <- subset(f2, select = -c(BL, LWL, HWL))
      
      BL <- f2$BL
      LWL <- f2$LWL
      HWL <- f2$HWL
      
      y <- data.frame(BL, LWL, HWL)
    
      c <- data.frame(data6())
      
      k <- y$BL*2^2+y$LWL*2^1+y$HWL*2^0
      k <- data.frame(k)
      for (i in 1:nrow(x))
      {
        if(k[i,1] == 4)
        {k[i,1] <- "Base_Line"}
        else if(k[i,1] == 2)
        {k[i,1] <- "Low_Work_Load"}
        else if(k[i,1] == 1)
        {k[i,1] <- "High_Work_Load"}
      }
      print(k)
      z <- subset(f2, select = -c(BL,LWL,HWL))
      e <- data.frame(z,k)
      e$k <- as.factor(e$k)
      
      fit <- lda(e$k~., data=e , na.action="na.omit", CV=FALSE)
      print(fit)
      
      final <- predict(fit, c)$class
      print(final)
      final <- data.frame(final)
      print(final)
    })
    ###############################################################################################################
    
    output$plotlda64 <- renderPlot({
      f2 <- data()
      f2 <- data.frame(f2)
      x <- subset(f2, select = -c(BL, LWL, HWL))
      
      BL <- f2$BL
      LWL <- f2$LWL
      HWL <- f2$HWL
      
      y <- data.frame(BL, LWL, HWL)
      
      c <- data.frame(data6())
      
      k <- y$BL*2^2+y$LWL*2^1+y$HWL*2^0
      k <- data.frame(k)
      for (i in 1:nrow(x))
      {
        if(k[i,1] == 4)
        {k[i,1] <- "Base_Line"}
        else if(k[i,1] == 2)
        {k[i,1] <- "Low_Work_Load"}
        else if(k[i,1] == 1)
        {k[i,1] <- "High_Work_Load"}
      }
      
      z <- subset(f2, select = -c(BL,LWL,HWL))
      e <- data.frame(z,k)
      e$k <- as.factor(e$k)
      
      fit <- lda(e$k~., data=e , na.action="na.omit", CV=FALSE)
      
      final <- predict(fit, c)$class
      
      final <- data.frame(final)
      
      par(mfrow = c(2,1))
      plot(fit, dimen=1, type="both", cex = 1.5)
      
    })
    
    ###############################################################################################################################################
    
    output$braintext <- renderText({
      
    "                       Here the plot shows the positioning of EEG electrodes for 14 channel EEG Machine"  
      
    })
    
    ###############################################################################################################################################
    
    output$brainplot <- renderPlot({
      
      eegcap(type = "2d")
    
    })
    
    ###############################################################################################################################################
    
    output$braintext64 <- renderText({
      
      "                 
                          Here the plot shows the positioning of EEG electrodes for 64 channel EEG Machine"  
      
    })
    
    ###############################################################################################################################################
    
    output$brainplot64 <- renderPlot({
      eegcap(type = "2d")
      
    })
    
    ###############################################################################################################################################
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$tb <- renderUI({
      if(is.null(data()))
        h5(tags$img(src="brain.png"), height=150, width=300)
      else{
        if(input$f14 == TRUE)
        {
        tabsetPanel(
          tabPanel("About the Project", verbatimTextOutput("filedf")),
          tabPanel("Artifical Neural Networks (ANN)", plotOutput("plotann"),  tableOutput("sample"), tableOutput("ann"), tableOutput("table")),
          tabPanel("Support Vector Machines (SVM)", tableOutput("svm"),  tableOutput("sample1"), tableOutput("table1")),
          tabPanel("Stacked Autoencoders (SAEs)", tableOutput("sda"), tableOutput("sample2"),tableOutput("table2")),
          tabPanel("Radial Basic Function (RBF)", tableOutput("rbf"), plotOutput("rbfplot"), tableOutput("sample3"), tableOutput("table3")),
          tabPanel("Linear Discriminant Analysis (LDA)", tableOutput("lda"), plotOutput("plotlda"), tableOutput("sample4"), tableOutput("table4")),
          tabPanel("Correlation Analysis", verbatimTextOutput("crtext"), tableOutput("cr1"), tableOutput("cr2"), tableOutput("cr3"), verbatimTextOutput("cr2text"), tableOutput("crr1"), tableOutput("crr2"), tableOutput("crr3")),
          tabPanel("Visualization", verbatimTextOutput("braintext"), plotOutput("brainplot")))
        }
        else if(input$f64 == TRUE)
        {
          tabsetPanel(
            tabPanel("About the Project", verbatimTextOutput("filedf")),
            tabPanel("Artifical Neural Networks (ANN)", plotOutput("plotann64"),  tableOutput("sample64"), tableOutput("ann64"), tableOutput("table64")),
            tabPanel("Support Vector Machines (SVM)", tableOutput("svm64"),  tableOutput("sample164"), tableOutput("table164")),
            tabPanel("Stacked Autoencoders (SAEs)", tableOutput("sda64"), tableOutput("sample264"),tableOutput("table264")),
            tabPanel("Radial Basic Function (RBF)", tableOutput("rbf64"), plotOutput("rbfplot64"), tableOutput("sample364"), tableOutput("table364")),
            tabPanel("Linear Discriminant Analysis (LDA)", tableOutput("lda64"), plotOutput("plotlda64"), tableOutput("sample464"), tableOutput("table464")),
            tabPanel("Correlation Analysis", verbatimTextOutput("crtext"), tableOutput("cr1"), tableOutput("cr2"), tableOutput("cr3"), verbatimTextOutput("cr2text"), tableOutput("crr1"), tableOutput("crr2"), tableOutput("crr3")),
          tabPanel("Visualization", verbatimTextOutput("braintext64"), plotOutput("brainplot64")))
        }
      }
    })
    
    })














































