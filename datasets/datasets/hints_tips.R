data <- read.csv("all_loan_data.csv",header=TRUE)
dim(data)

y <- data$repay_fail
# 15% default

# Review covariates
# Can any be removed now?  Why?  e.g.: Consider "last_pymnt_amnt"

# Exploratory data analysis
boxplot(data$loan_amnt~y,ylab="Loan amount",xlab="Default")
boxplot(data$int_rate~y,ylab="Interest rate",xlab="Default")
# Really large interest rate...probably doesn't make sense!
ind <- which(data$int_rate > 80)
data <- data[-ind,]

y <- data$repay_fail

boxplot(data$funded_amnt~y,ylab="Funded amount",xlab="Default")

# Seems like personal loans
hist(data$loan_amnt,xlab="Loan amount")

# Look at linear assumption for logistic regression
x <- data$loan_amnt
g <- cut(x, breaks=quantile(x,seq(0,100,10)/100))
ym <- tapply(y, g, mean)
xm <- tapply(x, g, mean)

plot(xm,ym)
ymp <- log(ym/(1-ym))
plot(xm,ymp)

# Interest rate
x <- data$int_rate
g <- cut(x, breaks=quantile(x,seq(0,100,10)/100))
ym <- tapply(y, g, mean)
xm <- tapply(x, g, mean)

plot(xm,ym)
ymp <- log(ym/(1-ym))
plot(xm,ymp)

#installment
x <- data$installment
g <- cut(x, breaks=quantile(x,seq(0,100,10)/100))
ym <- tapply(y, g, mean)
xm <- tapply(x, g, mean)

plot(xm,ym)
ymp <- log(ym/(1-ym))
plot(xm,ymp)

#annual income
ind <- which(is.na(data$annual_inc)==1)
x <- data$annual_inc[-ind]
g <- cut(x, breaks=quantile(x,seq(0,100,20)/100))
ym <- tapply(y[-ind], g, mean)
xm <- tapply(x, g, mean)

plot(xm,ym)
ymp <- log(ym/(1-ym))
plot(xm,ymp)

# tables
q <- colSums(table(y,data$term))
q <- rbind(q,q)
table(y,data$term)/q
q <- colSums(table(y,data$home_ownership))
q <- rbind(q,q)
table(y,data$home_ownership)/q
q <- colSums(table(y,data$emp_length))
q <- rbind(q,q)
table(y,data$emp_length)/q
# NAs
sum(is.na(data$term))/length(y)
# Large number of NAs (probably could be removed)
sum(is.na(data$mths_since_last_delinq))/length(y) 
sum(data$emp_length=="n/a")/length(y)  #### NA denoted as "n/a"

# Use the above exploratory analysis to give you an idea about what the data are telling you are important for predicting
# default.  Also, search literature 

# Explore relationships between covariates
# Remove one of two which have very high or very low correlation (e.g: abs(cor)> 0.85)
ind <- c(4,5,6,8,9,12,19,20,22,23,24,25,26,28,29,30,31,32,34)
cmat <- cor(data[,ind],use="pairwise.complete.obs")
which(abs(cmat)>0.85,arr.ind = TRUE)

# Which ones should we remove?  

##########################################################################
#### Hints ###############################################################
##########################################################################
# Setting up your analysis:
##########################################################################
# Training data is used for deriving your final model 
# Training data is where you should apply validation
# Validation data is used only to confirm performance of
#     your final model on 'new' data
# So only consider the validation data right at the end of your analysis
##########################################################################
# Exploratory data analysis:
##########################################################################
# Initially consider which variables might be important.  Remove variables
#     that don't make sense for predicting default of new (potential) customers
# Explore relationships between covariates.  Remove covariates that are
#     very similar.  For example, you don't need both covariates which
#     share abs(correlation) > 0.85.  Pick one which is most suitable.
# Explore relationships between repayment failure and covariates (example code
#     above) to get a feel for data and what might be significant.  Also
#     is the relationship appropriate.  For example, for continuous
#     variables, are they roughly linear with log(p/(1-p))?  If not,
#     then what could you do?
# How to deal with missing data?  What are reasonable approaches?
##########################################################################
# Model selection:
##########################################################################
# Model selection procedures discussed in lectures are useful but also
#     consider Gini
# Which variables make sense?
##########################################################################
# Model validation
##########################################################################
# Cross-validation performance (assessing predicted versus observed data) 
# seems hugely important.  Suggest this  must be considered (on training data for
# testing models that may not be your final model)
# Also need cross-validation of final model on validation data
# Also need to compare your final model against benchmark.  Benchmark
#     performance is given as ROC curves, see Week 6 lecture on binomial regression
# for information on ROC curves
# Clearly discussed/demonstrate how well you think your model will perform into the future
# for predicting defaults
##########################################################################
# Address specific questions in the assignment
##########################################################################
##########################################################################
# Conclusion/discussion/future endeavours 
##########################################################################
# Lastly, take advantage of workshop/consultation time....
# You can ask questions directly applicable to your project including:
#    - Does this variable make sense or not for predicting repayment failure?
#    - How would I choose between these two models based on AIC, Gini, etc?


