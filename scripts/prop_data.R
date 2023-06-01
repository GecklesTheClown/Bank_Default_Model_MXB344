### Exploring for fixed model
## term

# Obtain number of people in each term
alltermsum <- count(all_loan_data, term)

# Obtain number of people that failed in each term
all_loan_data_fail <- filter(all_loan_data, repay_fail == "1")
alltermfailsum <- count(all_loan_data_fail, term)

# Obtain proportion and logit(proportion) of failure in each term
allprop_term <- alltermfailsum[,2] / alltermsum[,2]
allprop_term_logit <- logit(allprop_term)

# Obtain proportion and logit(proportion) of failure in each term
allproptermdf <- data.frame(term = alltermsum$term, frequency = allprop_term, logit_frequency = allprop_term_logit)

# Graph for all term
graph_allptermfail <- ggplot(allproptermdf, aes(x=term, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allproptermdf$term) +
  labs(x = "term",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_alllogitptermfail <- ggplot(allproptermdf, aes(x=term, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allproptermdf$term) +
  labs(x = "term",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_allptermfail ; graph_alllogitptermfail

## Emp_length

# Obtain number of people in each emp_length
allempsum <- count(all_loan_data, emp_length)

# Obtain number of people that failed in each emp_length
all_loan_data_fail <- filter(all_loan_data, repay_fail == "1")
allempfailsum <- count(all_loan_data_fail, emp_length)

# Obtain proportion and logit(proportion) of failure in each emp_length
allprop_emp_length <- allempfailsum[,2] / allempsum[,2]
allprop_emp_length_logit <- logit(allprop_emp_length)

# Obtain proportion and logit(proportion) of failure in each emp_length
allpropempdf <- data.frame(emp_length = allempsum$emp_length, frequency = allprop_emp_length, logit_frequency = allprop_emp_length_logit)

# Graph for all emp_length
graph_allpempfail <- ggplot(allpropempdf, aes(x=emp_length, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allpropempdf$emp_length) +
  labs(x = "Emp_Length",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_alllogitpempfail <- ggplot(allpropempdf, aes(x=emp_length, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allpropempdf$emp_length) +
  labs(x = "Emp_length",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_allpempfail ; graph_alllogitpempfail

## home_ownership

# Obtain number of people in each home_ownership
allhomesum <- count(all_loan_data, home_ownership)

# Obtain number of people that failed in each home_ownership
all_loan_data_fail <- filter(all_loan_data, repay_fail == "1")
allhomefailsum <- count(all_loan_data_fail, home_ownership)

# Obtain proportion and logit(proportion) of failure in each home_ownership
allprop_home_ownership <- allhomefailsum[,2] / allhomesum[,2]
allprop_home_ownership_logit <- logit(allprop_home_ownership)

# Obtain proportion and logit(proportion) of failure in each home_ownership
allprophomedf <- data.frame(home_ownership = allhomesum$home_ownership, frequency = allprop_home_ownership, logit_frequency = allprop_home_ownership_logit)

# Graph for all home_ownership
graph_allphomefail <- ggplot(allprophomedf, aes(x=home_ownership, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allprophomedf$home_ownership) +
  labs(x = "home_ownership",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_alllogitphomefail <- ggplot(allprophomedf, aes(x=home_ownership, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allprophomedf$home_ownership) +
  labs(x = "home_ownership",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_allphomefail ; graph_alllogitphomefail

## verification_status

# Obtain number of people in each verification_status
allverisum <- count(all_loan_data, verification_status)

# Obtain number of people that failed in each verification_status
all_loan_data_fail <- filter(all_loan_data, repay_fail == "1")
allverifailsum <- count(all_loan_data_fail, verification_status)

# Obtain proportion and logit(proportion) of failure in each verification_status
allprop_verification_status <- allverifailsum[,2] / allverisum[,2]
allprop_verification_status_logit <- logit(allprop_verification_status)

# Obtain proportion and logit(proportion) of failure in each verification_status
allpropveridf <- data.frame(verification_status = allverisum$verification_status, frequency = allprop_verification_status, logit_frequency = allprop_verification_status_logit)

# Graph for all verification_status
graph_allpverifail <- ggplot(allpropveridf, aes(x=verification_status, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allpropveridf$verification_status) +
  labs(x = "verification_status",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_alllogitpverifail <- ggplot(allpropveridf, aes(x=verification_status, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allpropveridf$verification_status) +
  labs(x = "verification_status",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_allpverifail ; graph_alllogitpverifail

## purpose

# Obtain number of people in each purpose
allpurpsum <- count(all_loan_data, purpose)

# Obtain number of people that failed in each purpose
all_loan_data_fail <- filter(all_loan_data, repay_fail == "1")
allpurpfailsum <- count(all_loan_data_fail, purpose)

# Obtain proportion and logit(proportion) of failure in each purpose
allprop_purpose <- allpurpfailsum[,2] / allpurpsum[,2]
allprop_purpose_logit <- logit(allprop_purpose)

# Obtain proportion and logit(proportion) of failure in each purpose
allproppurpdf <- data.frame(purpose = allpurpsum$purpose, frequency = allprop_purpose, logit_frequency = allprop_purpose_logit)

# Graph for all purpose
graph_allppurpfail <- ggplot(allproppurpdf, aes(x=purpose, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allproppurpdf$purpose) +
  labs(x = "purpose",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_alllogitppurpfail <- ggplot(allproppurpdf, aes(x=purpose, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = allproppurpdf$purpose) +
  labs(x = "purpose",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_allppurpfail ; graph_alllogitppurpfail

### Exploring for random effects model

## term

# Obtain number of people in each term
exttermsum <- count(ext_loan_data, term)

# Obtain number of people that failed in each term
ext_loan_data_fail <- filter(ext_loan_data, repay_fail == "1")
exttermfailsum <- count(ext_loan_data_fail, term)

# Obtain proportion and logit(proportion) of failure in each term
extprop_term <- exttermfailsum[,2] / exttermsum[,2]
extprop_term_logit <- logit(extprop_term)

# Obtain proportion and logit(proportion) of failure in each term
extproptermdf <- data.frame(term = exttermsum$term, frequency = extprop_term, logit_frequency = extprop_term_logit)

# Graph for ext term
graph_extptermfail <- ggplot(extproptermdf, aes(x=term, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extproptermdf$term) +
  labs(x = "term",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extlogitptermfail <- ggplot(extproptermdf, aes(x=term, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extproptermdf$term) +
  labs(x = "term",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extptermfail ; graph_extlogitptermfail

## Emp_length

# Obtain number of people in each emp_length
extempsum <- count(ext_loan_data, emp_length)

# Obtain number of people that failed in each emp_length
ext_loan_data_fail <- filter(ext_loan_data, repay_fail == "1")
extempfailsum <- count(ext_loan_data_fail, emp_length)

# Obtain proportion and logit(proportion) of failure in each emp_length
extprop_emp_length <- extempfailsum[,2] / extempsum[,2]
extprop_emp_length_logit <- logit(extprop_emp_length)

# Obtain proportion and logit(proportion) of failure in each emp_length
extpropempdf <- data.frame(emp_length = extempsum$emp_length, frequency = extprop_emp_length, logit_frequency = extprop_emp_length_logit)

# Graph for ext emp_length
graph_extpempfail <- ggplot(extpropempdf, aes(x=emp_length, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extpropempdf$emp_length) +
  labs(x = "Emp_Length",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extlogitpempfail <- ggplot(extpropempdf, aes(x=emp_length, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extpropempdf$emp_length) +
  labs(x = "Emp_length",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extpempfail ; graph_extlogitpempfail

## home_ownership

# Obtain number of people in each home_ownership
exthomesum <- count(ext_loan_data, home_ownership)

# Obtain number of people that failed in each home_ownership
ext_loan_data_fail <- filter(ext_loan_data, repay_fail == "1")
exthomefailsum <- count(ext_loan_data_fail, home_ownership)

# Obtain proportion and logit(proportion) of failure in each home_ownership
extprop_home_ownership <- exthomefailsum[,2] / exthomesum[,2]
extprop_home_ownership_logit <- logit(extprop_home_ownership)

# Obtain proportion and logit(proportion) of failure in each home_ownership
extprophomedf <- data.frame(home_ownership = exthomesum$home_ownership, frequency = extprop_home_ownership, logit_frequency = extprop_home_ownership_logit)

# Graph for ext home_ownership
graph_extphomefail <- ggplot(extprophomedf, aes(x=home_ownership, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extprophomedf$home_ownership) +
  labs(x = "home_ownership",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extlogitphomefail <- ggplot(extprophomedf, aes(x=home_ownership, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extprophomedf$home_ownership) +
  labs(x = "home_ownership",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extphomefail ; graph_extlogitphomefail

## verification_status

# Obtain number of people in each verification_status
extverisum <- count(ext_loan_data, verification_status)

# Obtain number of people that failed in each verification_status
ext_loan_data_fail <- filter(ext_loan_data, repay_fail == "1")
extverifailsum <- count(ext_loan_data_fail, verification_status)

# Obtain proportion and logit(proportion) of failure in each verification_status
extprop_verification_status <- extverifailsum[,2] / extverisum[,2]
extprop_verification_status_logit <- logit(extprop_verification_status)

# Obtain proportion and logit(proportion) of failure in each verification_status
extpropveridf <- data.frame(verification_status = extverisum$verification_status, frequency = extprop_verification_status, logit_frequency = extprop_verification_status_logit)

# Graph for ext verification_status
graph_extpverifail <- ggplot(extpropveridf, aes(x=verification_status, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extpropveridf$verification_status) +
  labs(x = "verification_status",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extlogitpverifail <- ggplot(extpropveridf, aes(x=verification_status, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extpropveridf$verification_status) +
  labs(x = "verification_status",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extpverifail ; graph_extlogitpverifail

## purpose

# Obtain number of people in each purpose
extpurpsum <- count(ext_loan_data, purpose)

# Obtain number of people that failed in each purpose
ext_loan_data_fail <- filter(ext_loan_data, repay_fail == "1")
extpurpfailsum <- count(ext_loan_data_fail, purpose)

# Obtain proportion and logit(proportion) of failure in each purpose
extprop_purpose <- extpurpfailsum[,2] / extpurpsum[,2]
extprop_purpose_logit <- logit(extprop_purpose)

# Obtain proportion and logit(proportion) of failure in each purpose
extproppurpdf <- data.frame(purpose = extpurpsum$purpose, frequency = extprop_purpose, logit_frequency = extprop_purpose_logit)

# Graph for ext purpose
graph_extppurpfail <- ggplot(extproppurpdf, aes(x=purpose, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extproppurpdf$purpose) +
  labs(x = "purpose",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extlogitppurpfail <- ggplot(extproppurpdf, aes(x=purpose, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = extproppurpdf$purpose) +
  labs(x = "purpose",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -45,vjust = 0,hjust = 0))

graph_extppurpfail ; graph_extlogitppurpfail


## Plot the proportion and logit proportion of failure for issue_d

# Obtain number of people in each issue_d
issuedsum <- count(ext_loan_data, issue_d, .drop = FALSE)

# Obtain number of people that failed in each issue_d
ext_issued_fail <- count(ext_loan_data, issue_d, repay_fail == "1")
ext_missingrowIF <- c("Jun-2007", "TRUE", "0")
ext_issued_fail <- rbind(ext_issued_fail, ext_missingrowIF)
ext_issued_fail <- t(ext_issued_fail)
ext_issued_fail <- as.data.frame(ext_issued_fail)
ext_issued_fail <- relocate(ext_issued_fail, V110, .after = V55)
ext_issued_fail <- t(ext_issued_fail)
ext_issued_fail <- as.data.frame(ext_issued_fail)
ext_issued_fail$`repay_fail == "1"` <- as.logical(ext_issued_fail$`repay_fail == "1"`)
ext_issued_fail$n <- as.numeric(ext_issued_fail$n)
issuedfailsumext <- filter(ext_issued_fail, ext_issued_fail$`repay_fail == "1"` == "TRUE")

# Obtain proportion and logit(proportion) of failure in each issue_d
prop_issue_d <- issuedfailsumext[,3] / issuedsum[,2]

# Obtain proportion and logit(proportion) of failure in each issue_d
prop_issue_d_logit <- logit(prop_issue_d)
propissueddf <- data.frame(issue_d = issuedsum$issue_d, frequency = prop_issue_d, logit_frequency = prop_issue_d_logit )

pissuedfail <- ggplot(propissueddf, aes(x=issue_d, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = propissueddf$issue_d) +
  labs(x = "issue_d",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -90,vjust = 0,hjust = 0))

logitpissuedfail <- ggplot(propissueddf, aes(x=issue_d, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = propissueddf$issue_d) +
  labs(x = "issue_d",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -90,vjust = 0,hjust = 0))

pissuedfail ; logitpissuedfail

## Plot the proportion and logit proportion of failure for zip_code

# Obtain number of people in each zip_code
zipcsum <- count(ext_loan_data, zip_code, .drop = FALSE)

# Obtain number of people that failed in each zip_code
ext_zipc_fail <- count(ext_loan_data, zip_code, repay_fail == "1", .drop = FALSE)
ext_zipc_fail <- filter(ext_zipc_fail, ext_zipc_fail$`repay_fail == "1"` == "TRUE")
ext_zipc_fail$`repay_fail == "1"` <- NULL
ext_zipc_fail2 <- anti_join(zipcsum, ext_zipc_fail, by = c("zip_code" = "zip_code"))
ext_zipc_fail2$n <- 0
ext_zipc_fail <- rbind(ext_zipc_fail, ext_zipc_fail2)
ext_zipc_fail$zip_code <- gsub("xx","",as.character(ext_zipc_fail$zip_code))
ext_zipc_fail <- ext_zipc_fail[order(as.integer(ext_zipc_fail$zip_code),decreasing = FALSE), ]
ext_zipc_fail$zip_code <- paste0(ext_zipc_fail$zip_code, 'xx')

# Obtain proportion and logit(proportion) of failure in each zip_code
prop_zip_code <- ext_zipc_fail[,2] / zipcsum[,2]

# Obtain proportion and logit(proportion) of failure in each zip_code
prop_zip_code_logit <- sapply(prop_zip_code, logit)
propzipcdf <- data.frame(zip_code = zipcsum$zip_code, frequency = prop_zip_code, logit_frequency = prop_zip_code_logit )

pzipcfail <- ggplot(propzipcdf, aes(x=zip_code, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = propzipcdf$zip_code) +
  labs(x = "zip_code",y = "Repay Failure Proportion") 

logitpzipcfail <- ggplot(propzipcdf, aes(x=zip_code, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = propzipcdf$zip_code) +
  labs(x = "zip_code",y = "Repay Failure logit(Proportion)") 

pzipcfail ; logitpzipcfail

# We did not explore the effects of zip code onto each variable since there 1 or very low number of observations in some zip codes

## Plot the proportion and logit proportion of failure for addr_state

# Obtain number of people in each addr_state
addssum <- count(ext_loan_data, addr_state, .drop = FALSE)

# Obtain number of people that failed in each addr_state
ext_adds_fail <- count(ext_loan_data, addr_state, repay_fail == "1")
ext_missingrowAF <- c("ME", "TRUE", "0")
ext_adds_fail <- rbind(ext_adds_fail, ext_missingrowAF)
ext_adds_fail <- t(ext_adds_fail)
ext_adds_fail <- as.data.frame(ext_adds_fail)
ext_adds_fail <- relocate(ext_adds_fail, V100, .after = V43)
ext_adds_fail <- t(ext_adds_fail)
ext_adds_fail <- as.data.frame(ext_adds_fail)
ext_adds_fail$`repay_fail == "1"` <- as.logical(ext_adds_fail$`repay_fail == "1"`)
ext_adds_fail$n <- as.numeric(ext_adds_fail$n)
addsfailsumext <- filter(ext_adds_fail, ext_adds_fail$`repay_fail == "1"` == "TRUE")

# Obtain proportion and logit(proportion) of failure in each addr_state
prop_addr_state <- addsfailsumext[,3] / addssum[,2]

# Obtain proportion and logit(proportion) of failure in each addr_state
prop_addr_state_logit <- sapply(prop_addr_state, logit)
propaddsdf <- data.frame(addr_state = addssum$addr_state, frequency = prop_addr_state, logit_frequency = prop_addr_state_logit )

paddsfail <- ggplot(propaddsdf, aes(x=addr_state, y=frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = propaddsdf$addr_state) +
  labs(x = "addr_state",y = "Repay Failure Proportion") +
  theme(axis.text.x = element_text(angle = -90,vjust = 0,hjust = 0))

logitpaddsfail <- ggplot(propaddsdf, aes(x=addr_state, y=logit_frequency)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  scale_x_discrete(labels = propaddsdf$addr_state) +
  labs(x = "addr_state",y = "Repay Failure logit(Proportion)") +
  theme(axis.text.x = element_text(angle = -90,vjust = 0,hjust = 0))

paddsfail ; logitpaddsfail