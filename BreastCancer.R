
library(caret) 

library(corrplot)

library(leaps)

library(bestglm)

library(MPV)

library(MASS)

library(dplyr)

  
  setwd("~/Drexel/Classes/Summer 2020 Quarter/Applied Regression/Breast Cancer")
  
  Breast_Cancer = read.csv(file.choose())


## Explore Data

  str(Breast_Cancer)


## Create numeric binary variable from Diagnosis

  Breast_Cancer$diagnosis = ifelse(Breast_Cancer$diagnosis =="B", 0, 1)

## Remove useless variables

  Breast_Cancer = subset(Breast_Cancer, select = -c(X, id))


## Remove any missing values

  Breast_Cancer = Breast_Cancer[complete.cases(Breast_Cancer), ]


  Breast_Cancer$diagnosis = as.factor(Breast_Cancer$diagnosis)


## Detect multicollinearity



    correlation_data = cor(Breast_Cancer[, -1])
    
    corrplot(correlation_data)


## Find highly correlated variables

## Dummies (Wiley) says that >= 0.7 shows strong correlation strength. 

## https://www.dummies.com/education/math/statistics/how-to-interpret-a-correlation-coefficient-r/

    high_correlation_vars = findCorrelation(correlation_data, cutoff = 0.7, names=TRUE)
    
    high_correlation_vars
    
    
    
    Breast_Cancer_DT = Breast_Cancer[, !names(Breast_Cancer) %in% high_correlation_vars] 
    
    
    sub <- createDataPartition(Breast_Cancer_DT$diagnosis, p=0.80, list=FALSE)
    
    Train  = Breast_Cancer_DT[sub,]
    
    Test = Breast_Cancer_DT[-sub,]


## Original Model

      Log_Model = model=glm(diagnosis~., family="binomial", Train)
      
      Predictions = predict(Log_Model, Test[, -1] , type = "response")
        
      Predictions = round(Predictions, 2)
    
    
    # cutoff value vs cost/profit (copy from Neel's code and modify cost part)
    i=1
    k=0.01 ## cutt off value
    tpr={}
    fpr={}
    accuracy={}
    m={}
    profit={}
    ## run a while loop for cuttoff values 0.01 to 1
    
    while( k <= 1){  
      classify=ifelse(Predictions> k,1,0) 
      acc=c_accuracy(Test$diagnosis,classify)
      Reward = 100   
      Loss= 100
     
      
      profit[i]= ((Reward) * (acc[7] + acc[8])) - (( Loss) * (acc[9] + acc[10]))  #care about net-profit only
      
      tpr[i] = acc[4]
      fpr[i]=acc[5]
      accuracy[i]= acc[3]
      m[i]=k
      
      
      
      k=k+0.01 ## increment the cuttoff value by 0.01
      
      i=i+1 ## i represents the index of the cutoff value. 
      
      
      
    }
    
    plot(m, accuracy, xlab="cutoff value", main = "Original Model: Cut off value vs Accuracy")
    
    lines(m, accuracy, col="blue")
    
    
    plot(m, profit, xlab="cutoff value", main = " Original Model: Cut off value vs Profit")
    
    lines(m, profit, col="red")
    
    c_accuracy(Test$diagnosis, ifelse(Predictions> 0.53,1,0))
                   
    ## Find out the highest accuracy
    
    which.max(accuracy) *0.01
    
    which.max(profit) * 0.01
    
    
    
    
    
    ## Function Below, RUN THIS FIRST
    ## make sure actuals and classifications are 0 (no) or 1 (yes) only 
    ##  Built by Matthew J. Schneider
    
    c_accuracy=function(actuals,classifications){
      df=data.frame(actuals,classifications);
      
      
      tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
      fp=nrow(df[df$classifications==1 & df$actuals==0,]);
      fn=nrow(df[df$classifications==0 & df$actuals==1,]);
      tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
      
      
      recall=tp/(tp+fn)
      precision=tp/(tp+fp)
      accuracy=(tp+tn)/(tp+fn+fp+tn)
      tpr=recall
      fpr=fp/(fp+tn)
      fmeasure=2*precision*recall/(precision+recall)
      scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
      names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
      
      #print(scores)
      return(scores);
    }
    

## N best Selection Method


    regsubsets.out = regsubsets(diagnosis ~. ,
                 data = Breast_Cancer_DT,
                 nbest = 3,       # 1 best model for each number of predictors
                 nvmax =5,    # NULL for no limit on number of variables
                 force.in = NULL, force.out = NULL,
                 method = "exhaustive")
    regsubsets.out
    
    summary(regsubsets.out) ## summary of all the 15 models
    
    summary(regsubsets.out)$bic
    
    summary(regsubsets.out)$cp
    
    plot(1:15, summary(regsubsets.out)$bic, main = "Reg Subsets - BIC by model #", ylab= "BIC", xlab = "Model #")
    lines(summary(regsubsets.out)$bic, col= "red")
    
    
    plot(1:15, summary(regsubsets.out)$cp, main = "Reg Subsets - Mallow's CP by model #", ylab= "Mallow's CP", xlab = "Model #")
    lines(summary(regsubsets.out)$cp, col= 'blue')
    
    coef(regsubsets.out,10)
    
    coef(regsubsets.out,13)
    coef(regsubsets.out,14)
    coef(regsubsets.out,15)
    
    RS_Model10 = glm(diagnosis~ texture_mean + area_mean + smoothness_worst + symmetry_worst, family="binomial", Breast_Cancer_DT)
    
    RS_Model13 = glm(diagnosis~ texture_mean + area_mean + smoothness_worst + symmetry_worst + fractal_dimension_worst, family="binomial", Breast_Cancer_DT)
    
    RS_Model14 = glm(diagnosis~ texture_mean + area_mean + symmetry_se +  smoothness_worst + symmetry_worst, family="binomial", Breast_Cancer_DT)
    
    RS_Model15 = glm(diagnosis~ texture_mean + area_mean + smoothness_se + smoothness_worst + symmetry_worst, family = "binomial", Breast_Cancer_DT)
    
    
    
    RS_Log_Model = glm(diagnosis~ texture_mean + area_mean + smoothness_worst + symmetry_worst + fractal_dimension_worst, family="binomial", Train)
    
    RS_Predictions = predict(RS_Log_Model, Test[, -1], type = "response")
    
    
    
    
    i=1
    k=0.01 ## cutt off value
    tpr={}
    fpr={}
    accuracy={}
    m={}
    
    ## run a while loop for cuttoff values 0.01 to 1
    
    while( k <= 1){  
      classify=ifelse(RS_Predictions> k,1,0) 
      acc=c_accuracy(Test$diagnosis,classify)
      Reward = 100   
      Loss= 100
      
      
      profit[i]= ((Reward) * (acc[7] + acc[8])) - (( Loss) * (acc[9] + acc[10]))  #care about net-profit only
      
      tpr[i] = acc[4]
      fpr[i]=acc[5]
      accuracy[i]= acc[3]
      m[i]=k
      
      
      
      k=k+0.01 ## increment the cuttoff value by 0.01
      
      i=i+1 ## i represents the index of the cutoff value. 
      
      
      
    }
    
    plot(m, accuracy, xlab="cutoff value", main = "Reg Subsets Model #13: Cut off value vs Accuracy")
    
    lines(m, accuracy, col="green")
    
    
    plot(m, profit, xlab="cutoff value", main = "Reg Subsets Model #13 : Cut off value vs Profit")
    
    lines(m, profit, col="purple")
    
    
    ## Find out the highest accuracy
    
    which.max(accuracy) *0.01
    
    which.max(profit) * 0.01
    
    
    c_accuracy(Test$diagnosis, ifelse(RS_Predictions> 0.67,1,0) )



## Add one - Drop one
    
    empty<-glm(diagnosis~1,family= "binomial", Breast_Cancer_DT)
    full<- glm(diagnosis~texture_mean+area_mean+symmetry_mean+texture_se+smoothness_se+symmetry_se+fractal_dimension_se+smoothness_worst+symmetry_worst+fractal_dimension_worst, family = 'binomial', Breast_Cancer_DT)
    
    add1(empty,scope=full,test="LRT")
    
    
    drop1(update(empty,~.+area_mean, Breast_Cancer_DT), test="LRT")
    
    
    add1(update(empty,~.+area_mean, Breast_Cancer_DT),scope=full,test="LRT")
    
    drop1(update(empty,~.+smoothness_worst + area_mean, Breast_Cancer_DT), test="LRT")
    
    add1(update(empty,~.+smoothness_worst +area_mean, Breast_Cancer_DT),scope=full,test="LRT")
    
    
    drop1(update(empty,~.+smoothness_worst + area_mean + texture_mean, Breast_Cancer_DT), test="LRT")
    
    add1(update(empty,~.+smoothness_worst +area_mean + texture_mean, Breast_Cancer_DT),scope=full,test="LRT")
    
    
    drop1(update(empty,~.+smoothness_worst + area_mean + texture_mean + symmetry_worst, Breast_Cancer_DT), test="LRT")
    
    add1(update(empty,~.+smoothness_worst +area_mean + texture_mean + symmetry_worst, Breast_Cancer_DT),scope=full,test="LRT")
    
    drop1(update(empty,~.+smoothness_worst + area_mean + texture_mean + symmetry_worst + texture_se, Breast_Cancer_DT), test="LRT")
    
    drop1(update(empty,~.+smoothness_worst + area_mean + texture_mean + symmetry_worst, Breast_Cancer_DT), test="LRT")
    
    
    Drop1Model = glm(diagnosis ~ smoothness_worst + area_mean + texture_mean + symmetry_worst, family = "binomial",  Breast_Cancer_DT)
    
    
    Drop1Model = glm(diagnosis ~ smoothness_worst + area_mean + texture_mean + symmetry_worst, family = "binomial",  Train)
    
    D1M_Predictions = predict(Drop1Model, Test[, -1] , type = "response")   ## Replace Log_Model with your Model name
    
    D1M_Predictions = round(D1M_Predictions, 2)
    
    
    
    i=1
    k=0.01 ## cutt off value
    tpr={}
    fpr={}
    accuracy={}
    m={}
    profit ={}
    
    ## run a while loop for cuttoff values 0.01 to 1
    
    while( k <= 1){  
      classify=ifelse(D1M_Predictions> k,1,0) 
      acc=c_accuracy(Test$diagnosis,classify)
      Reward = 100   
      Loss= 100
      
      
      profit[i]= ((Reward) * (acc[7] + acc[8])) - (( Loss) * (acc[9] + acc[10]))  #care about net-profit only
      
      tpr[i] = acc[4]
      fpr[i]=acc[5]
      accuracy[i]= acc[3]
      m[i]=k
      
      
      
      k=k+0.01 ## increment the cuttoff value by 0.01
      
      i=i+1 ## i represents the index of the cutoff value. 
      
      
      
    }
    
    plot(m, accuracy, xlab="cutoff value", main = "Add One Drop One Model : Cut off value vs Accuracy")
    
    lines(m, accuracy, col="blue")
    
    
    plot(m, profit, xlab="cutoff value", main = "Add One Drop One : Cut off value vs Profit")
    
    lines(m, profit, col="red")
    
    
    ## Find out the highest accuracy
    
    which.max(accuracy) *0.01
    
    which.max(profit) * 0.01
    
    
    c_accuracy(Test$diagnosis, ifelse(D1M_Predictions> 0.69,1,0))
    
    ## Function Below, RUN THIS FIRST
    ## make sure actuals and classifications are 0 (no) or 1 (yes) only 
    ##  Built by Matthew J. Schneider
    
    c_accuracy=function(actuals,classifications){
      df=Breast_Cancer_DT.frame(actuals,classifications);
      
      
      tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
      fp=nrow(df[df$classifications==1 & df$actuals==0,]);
      fn=nrow(df[df$classifications==0 & df$actuals==1,]);
      tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
      
      
      recall=tp/(tp+fn)
      precision=tp/(tp+fp)
      accuracy=(tp+tn)/(tp+fn+fp+tn)
      tpr=recall
      fpr=fp/(fp+tn)
      fmeasure=2*precision*recall/(precision+recall)
      scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
      names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
      
      #print(scores)
      return(scores);
    }


## Stepwise selection methods (Backwards, Foreward, Stepwise)
    

    #loading the data set
    
   
  
    #stepAIC forward, backward and both
    empty <- glm(diagnosis~1,family  = "binomial", Breast_Cancer_DT = Breast_Cancer_DT)
    full<- glm(diagnosis~texture_mean+area_mean+symmetry_mean+texture_se+smoothness_se+symmetry_se+fractal_dimension_se+smoothness_worst+symmetry_worst+fractal_dimension_worst,family = "binomial", Breast_Cancer_DT)
    
    
    
    stepAIC(empty,direction='forward',scope=list(lower=empty,upper=full))
    
    stepAIC(empty,direction='both',scope=list(lower=empty,upper=full))
    
    stepAIC(full,direction='backward',scope=list(lower=empty,upper=full))
    
    
    
   ## Creating the Forward, Backward and Stepwise models using the Entire Dataset 
    
    modelforward <- glm(formula = diagnosis ~ area_mean + smoothness_worst + texture_mean + 
                          symmetry_worst, family = "binomial",  Breast_Cancer_DT)
    summary(modelforward)
    
    modelboth<-glm(formula = diagnosis ~ area_mean + smoothness_worst + texture_mean + 
                     symmetry_worst, family = "binomial",  Breast_Cancer_DT)
    
    summary(modelboth)
    
    modelbackward<- glm(formula = diagnosis ~ texture_mean + area_mean + texture_se + 
                          fractal_dimension_se + smoothness_worst + symmetry_worst + 
                          fractal_dimension_worst, family = "binomial",  Breast_Cancer_DT)
    
    summary(modelbackward)
    
    modelbackward

    ## Choosing the backwards best model and fitting it on training data
  
    modelbackward<- glm(formula = diagnosis ~ texture_mean + area_mean + texture_se + 
                          fractal_dimension_se + smoothness_worst + symmetry_worst + 
                          fractal_dimension_worst, family = "binomial", data = Train)
    
    ## Use the backward model to predict on test test using training data
    
    Predictions = predict(modelbackward, Test[, -1] , type = "response")   ## Replace Log_Model with your Model name
    
    Predictions = round(Predictions, 2)
    
    
    # cutoff value vs cost/profit (copy from Neel's code and modify cost part)
    i=1
    k=0.01 ## cutt off value
    tpr={}
    fpr={}
    accuracy={}
    m={}
    profit = {}
    
    ## run a while loop for cuttoff values 0.01 to 1
    
    while( k <= 1){  
      classify=ifelse(Predictions> k,1,0) 
      acc=c_accuracy(Test$diagnosis,classify)
      Reward = 100   
      Loss= 100
      
      
      profit[i]= ((Reward) * (acc[7] + acc[8])) - (( Loss) * (acc[9] + acc[10]))  #care about net-profit only
      
      tpr[i] = acc[4]
      fpr[i]=acc[5]
      accuracy[i]= acc[3]
      m[i]=k
      
      
      
      k=k+0.01 ## increment the cuttoff value by 0.01
      
      i=i+1 ## i represents the index of the cutoff value. 
      
      
      
    }
    
    plot(m, accuracy, xlab="cutoff value", main = "Backwards Selection Model:Cut off value vs Accuracy")
    
    lines(m, accuracy, col="blue")
    
    
    plot(m, profit, xlab="cutoff value", main = "Backwards Selection Model: Cut off value vs Profit")
    
    lines(m, profit, col="red")
  
    
    ## Find out the highest accuracy
    
    which.max(accuracy) *0.01
    
    which.max(profit) * 0.01
    
    c_accuracy(Test$diagnosis, ifelse(Predictions> 0.47,1,0))
    
    
    
    ## Function Below, RUN THIS FIRST
    ## make sure actuals and classifications are 0 (no) or 1 (yes) only 
    ##  Built by Matthew J. Schneider
    
    c_accuracy=function(actuals,classifications){
      df=data.frame(actuals,classifications);
      
      
      tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
      fp=nrow(df[df$classifications==1 & df$actuals==0,]);
      fn=nrow(df[df$classifications==0 & df$actuals==1,]);
      tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
      
      
      recall=tp/(tp+fn)
      precision=tp/(tp+fp)
      accuracy=(tp+tn)/(tp+fn+fp+tn)
      tpr=recall
      fpr=fp/(fp+tn)
      fmeasure=2*precision*recall/(precision+recall)
      scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
      names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
      
      #print(scores)
      return(scores);
    }
    
    Final_Model = RS_Log_Model

#DEVIANCES
    
    
    summary(Final_Model)
    summary(residuals(Final_Model))

    load(file = "Enviornment.RData")
    