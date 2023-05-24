roc.glm <- function(m) { 
  
  plot <- predict(m, 
                  type = 'response', 
                  newdata = training_loan_data) %>% 
    
    prediction(training_loan_data$repay_fail) %>%
    
    performance(measure = "tpr", x.measure = "fpr")
  
  auc_holder <- predict(m, 
                        type = 'response', 
                        newdata = training_loan_data) %>% 
    
    prediction(training_loan_data$repay_fail) %>%
    
    performance(measure = "auc")
  
  plot2 <- predict(m, 
                   type = 'response', 
                   newdata = validation_loan_data) %>% 
    
    prediction(validation_loan_data$repay_fail) %>%
    
    performance(measure = "tpr", x.measure = "fpr")
  
  auc_holder2 <- predict(m, 
                         type = 'response', 
                         newdata = validation_loan_data) %>% 
    
    prediction(validation_loan_data$repay_fail) %>%
    
    performance(measure = "auc")
  
  par(mfrow=c(1,2))
  plot(plot)
  title("Training Data")
  abline(a=0, b= 1)
  plot(plot2)
  title("Validation Data")
  abline(a=0, b= 1)
  
  df_holder <- data.frame(Training = auc_holder@y.values,
                          Validation = auc_holder2@y.values)
  
  colnames(df_holder) <- c("Training", "Validation")
  
  df_holder
}

roc.glmm <- function(m) { 
  
  plot <- predict(m, 
                  type = 'response', 
                  newdata = extended_loan_data) %>% 
    
    prediction(extended_loan_data$repay_fail) %>%
    
    performance(measure = "tpr", x.measure = "fpr")
  
  
  plot(plot)
  abline(a=0, b= 1)
  
  auc_holder <- predict(m, 
                        type = 'response', 
                        newdata = extended_loan_data) %>% 
    
    prediction(extended_loan_data$repay_fail) %>%
    
    performance(measure = "auc")
  
  auc_holder@y.values
  
}
