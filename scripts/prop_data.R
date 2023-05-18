## Term
termsum <- count(all_loan_data, term)

# Obtain number of people that failed in each term
loan_data_fail <- filter(all_loan_data, repay_fail == "1")
termfailsum <- count(loan_data_fail, term)

# Obtain proportion and log(proportion) of failure in each term
PropM36F <- termfailsum$n[1] / termsum$n[1]
PropM60F <- termfailsum$n[2] / termsum$n[2]
logPropM36F <- log(PropM36F)
logPropM60F <- log(PropM60F)

# Create data frame containing proportion and log(proportion) of failure in each term
propterm <- c(PropM36F, PropM60F)
logpropterm <- c(logPropM36F, logPropM60F)
proptermdf <- as.table(propterm)
proptermdf <- as.data.frame(proptermdf)
proptermdf <- cbind(proptermdf, logpropterm)
term_labels <- c("36 Months", "60 Months")


## Emp_length

# Obtain number of people in each emp_length
empsum <- count(all_loan_data, emp_length)

# Obtain number of people that failed in each emp_length
empfailsum <- count(loan_data_fail, emp_length)

# Obtain proportion and log(proportion) of failure in each emp_length
prop_emp_length <- empfailsum[,2] / empsum[,2]

# Obtain proportion and log(proportion) of failure in each emp_length

propempdf <- data.frame(emp_length = empsum$emp_length, frequency = prop_emp_length, log_frequency = prop_emp_length_log )


## home ownership

homesum <- count(all_loan_data, home_ownership)
homefailsum <- count(loan_data_fail, home_ownership) # Obtain number of people that failed in each home_ownership
prop_home_ownership <- homefailsum[,2] / homesum[,2] # Obtain proportion and log(proportion) of failure in each home_ownership
prop_home_ownership_log <- sapply(prop_home_ownership, log)
df_prop_home <- data.frame(home_ownership = homesum$home_ownership, frequency = prop_home_ownership, log_frequency = prop_home_ownership_log )


## Verification Status

# verisum <- summary(all_loan_data$verification_status)
# NotVeri <- verisum[1]
# SouVeri <- verisum[2]
# Veri <- verisum[3]
# # Obtain number of people that failed in each Verification_status
# verifail <- loan_data_fail$verification_status
# verifailsum <- summary(verifail)
# NotVeriF <- verifailsum[1]
# SouVeriF <- verifailsum[2]
# VeriF <- verifailsum[3]
# # Obtain proportion and log(proportion) of failure in each Verification_status
# PropNotVeriF <- NotVeriF / NotVeri
# PropSouVeriF <- SouVeriF / SouVeri
# PropVeriF <- VeriF / Veri
# LogPropNotVeriF <- log(PropNotVeriF)
# LogPropSouVeriF <- log(PropSouVeriF)
# LogPropVeriF <- log(PropVeriF)
# # Create data frame containing proportion and log(proportion) of failure in each Verification_status
# propveri <- c(PropNotVeriF, PropSouVeriF, PropVeriF)
# logpropveri <- c(LogPropNotVeriF, LogPropSouVeriF, LogPropVeriF)
# propveridf <- as.table(propveri)
# propveridf <- as.data.frame(propveridf)
# propveridf <- cbind(propveridf, logpropveri)
# # Graph the data
# veri_labels <- c("Not Verified", "Source Verified", "Verified") # Make labels for the box plots with Verification_status as a variable
# 
# ## Purpose
# purpsum <- summary(all_loan_data$purpose)
# Car <- purpsum[1]
# Cred <- purpsum[2]
# Dept <- purpsum[3]
# Edu <- purpsum[4]
# Home <- purpsum[5]
# House <- purpsum[6]
# Maj <- purpsum[7]
# Med <- purpsum[8]
# Move <- purpsum[9]
# Oth <- purpsum[10]
# Renew <- purpsum[11]
# Small <- purpsum[12]
# Vac <- purpsum[13]
# Wed <- purpsum[14]
# # Obtain number of people that failed in each purpose
# purpfail <- loan_data_fail$purpose
# purpfailsum <- summary(purpfail)
# CarF <- purpfailsum[1]
# CredF <- purpfailsum[2]
# DeptF <- purpfailsum[3]
# EduF <- purpfailsum[4]
# HomeF <- purpfailsum[5]
# HouseF <- purpfailsum[6]
# MajF <- purpfailsum[7]
# MedF <- purpfailsum[8]
# MoveF <- purpfailsum[9]
# OthF <- purpfailsum[10]
# RenewF <- purpfailsum[11]
# SmallF <- purpfailsum[12]
# VacF <- purpfailsum[13]
# WedF <- purpfailsum[14]
# # Obtain proportion and log(proportion) of failure in each purpose
# PropCarF <- CarF / Car
# PropCredF <- CredF / Cred
# PropDeptF <- DeptF / Dept
# PropEduF <- EduF / Edu
# PropHomeF <- HomeF / Home
# PropHouseF <- HouseF / House
# PropMajF <- MajF / Maj
# PropMedF <- MedF / Med
# PropMoveF <- MoveF / Move
# PropOthF <- OthF / Oth
# PropRenewF <- RenewF / Renew
# PropSmallF <- SmallF / Small
# PropVacF <- VacF / Vac
# PropWedF <- WedF / Wed
# LogPropCarF <- log(PropCarF)
# LogPropCredF <- log(PropCredF)
# LogPropDeptF <- log(PropDeptF)
# LogPropEduF <- log(PropEduF)
# LogPropHomeF <- log(PropHomeF)
# LogPropHouseF <- log(PropHouseF)
# LogPropMajF <- log(PropMajF)
# LogPropMedF <- log(PropMedF)
# LogPropMoveF <- log(PropMoveF )
# LogPropOthF <- log(PropOthF)
# LogPropRenewF <- log(PropRenewF)
# LogPropSmallF <- log(PropSmallF)
# LogPropVacF <- log(PropVacF)
# LogPropWedF <- log(PropWedF)
# # Create data frame containing proportion and log(proportion) of failure in each purpose
# proppurp <- c(PropCarF,PropCredF,PropDeptF,PropEduF,PropHomeF,PropHouseF,PropMajF,PropMedF,PropMoveF,PropOthF,PropRenewF,PropSmallF,PropVacF,PropWedF)
# logproppurp <- c(LogPropCarF,LogPropCredF,LogPropDeptF,LogPropEduF,LogPropHomeF,LogPropHouseF,LogPropMajF,LogPropMedF,LogPropMoveF,LogPropOthF,LogPropRenewF,LogPropSmallF,LogPropVacF,LogPropWedF)
# proppurpdf <- as.table(proppurp)
# proppurpdf <- as.data.frame(proppurpdf)
# proppurpdf <- cbind(proppurpdf, logproppurp)
# # Graph the data
# purp_labels <- c("Car", "Credit_card", "Dept_Consilidation","Educational","Home_Improvement","House","Major_Purchase","Medical","Moving","Other","Renewable_energy","Small_Business","Vacation","Wedding")


# Graphs 

graph_ptermfail <- ggplot(proptermdf, aes(x=Var1, y=Freq)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = term_labels) +
  labs(x = "Term",y = "Repay Failure Proportion") 

graph_logptermfail <- ggplot(proptermdf, aes(x=Var1, y=logpropterm)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = term_labels) +
  labs(x = "Term",y = "Repay Failure Log(Proportion)") 

graph_pempfail <- ggplot(propempdf, aes(x=emp_length, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = propempdf$emp_length) +
  labs(x = "Emp_Length",y = "Repay Failure Proportion") 

graph_logpempfail <- ggplot(propempdf, aes(x=emp_length, y=log_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = propempdf$emp_length) +
  labs(x = "Emp_length",y = "Repay Failure Log(Proportion)")

graph_phomefail<-ggplot(df_prop_home, aes(x=home_ownership, y=prop_home_ownership)) +
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = df_prop_home$home_ownership) +
  labs(x = "Home_Ownership",y = "Repay Failure Proportion")

graph_logphomefail<-ggplot(df_prop_home, aes(x=home_ownership, y=prop_home_ownership_log)) +
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = df_prop_home$home_ownership) +
  labs(x = "Home_Ownership",y = "Repay Failure Log(Proportion)")

# pverifail<-ggplot(propveridf, aes(x=Var1, y=Freq)) + 
#   geom_dotplot(binaxis='y', stackdir='center') +
#   scale_x_discrete(labels = veri_labels) +
#   labs(x = "Verification_Status",y = "Repay Failure Proportion") 
# 
# logpverifail<-ggplot(propveridf, aes(x=Var1, y=logpropveri)) + 
#   geom_dotplot(binaxis='y', stackdir='center') +
#   scale_x_discrete(labels = veri_labels) +
#   labs(x = "Verification_Status",y = "Repay Failure Log(Proportion)") 
# 
# ppurpfail<-ggplot(proppurpdf, aes(x=Var1, y=Freq)) + 
#   geom_dotplot(binaxis='y', stackdir='center') +
#   scale_x_discrete(labels = purp_labels) +
#   labs(x = "Purpose",y = "Repay Failure Proportion") +
#   theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))
# 
# logppurpfail<-ggplot(proppurpdf, aes(x=Var1, y=logproppurp)) + 
#   geom_dotplot(binaxis='y', stackdir='center') +
#   scale_x_discrete(labels = purp_labels) +
#   labs(x = "Purpose",y = "Repay Failure Log(Proportion)") +
#   theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))
