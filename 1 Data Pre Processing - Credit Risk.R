# ===== DATA PRE PROCESSING - DATA MINING =====

library(mice)
library(car)

# ===== READ DATA
data.train = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Training.csv',
                              header = TRUE,
                              sep = ',')
View(data.train)
str(data.train)
summary(data.train)
unique(data.train$ID_CLIENT)
colnames(data.train)

# ===== CHECK MISSING VALUES
table(is.na(data.train)) # NA in Education Column

# ===== DROP SOME VARIABLES
# Drop ID_CLIENT and ID_SHOP
data.train = data.train[,-c(1,2)]
# Drop EDUCATION
summary(data.train$EDUCATION) # All rows contains NA
data.train = data.train[,-5]
# Drop PROFESSION_CODE
summary(data.train$PROFESSION_CODE)
unique(data.train$PROFESSION_CODE) # There are 291 professions in dataset without description
data.train = data.train[,-16]
# Drop COD_APPLICATION_BOOTH
summary(data.train$COD_APPLICATION_BOOTH) # All rows contains NULL
data.train = data.train[,-25]
# Drop QUANT_BANKING_ACCOUNTS
summary(data.train$QUANT_BANKING_ACCOUNTS) # All rows contains NULL
data.train = data.train[,-19]

# Manipulation PERSONAL_REFERENCE_.1
data.train$PERSONAL_REFERENCE_.1
table(data.train$PERSONAL_REFERENCE_.1 == '') # Check if PERSONAL_REFERENCE_.1 contains ''
index.null = which(data.train$PERSONAL_REFERENCE_.1 == '') # Index for rows that contain NULL or ''
data.train$PERSONAL_REFERENCE_.1 = 1 # If applicant fill personal reference
data.train[index.null,'PERSONAL_REFERENCE_.1'] = 0 # If applicant do not fill personal reference
unique(data.train$PERSONAL_REFERENCE_.1)
summary(data.train$PERSONAL_REFERENCE_.1)
# Manipulation PERSONAL_REFERENCE_.2
data.train$PERSONAL_REFERENCE_.2
table(data.train$PERSONAL_REFERENCE_.2 == '') # Check if PERSONAL_REFERENCE_.1 contains ''
index.null = which(data.train$PERSONAL_REFERENCE_.2 == '') # Index for rows that contain NULL or ''
data.train$PERSONAL_REFERENCE_.2 = 1 # If applicant fill personal reference
data.train[index.null,'PERSONAL_REFERENCE_.2'] = 0 # If applicant do not fill personal reference
unique(data.train$PERSONAL_REFERENCE_.2)
table(data.train$PERSONAL_REFERENCE_.2)

# Drop FLAG_CARD_INSURANCE_OPTION
summary(data.train$FLAG_CARD_INSURANCE_OPTION) # All rows contain 'N', so it's dropped
data.train = data.train[,-25]
# Drop QUANT_DEPENDANTS
summary(data.train$QUANT_DEPENDANTS) # All rows contain '0', so it's dropped
data.train = data.train[,-4]
# Drop AREA_CODE_RESIDENCIAL_PHONE and Save as index
data.train = data.train[,-5]

# Manipulation SEX
unique(data.train$SEX)
index.sex.null = which(data.train$SEX == '')
data.train = data.train[-index.sex.null,]
# Manipulation PAYMENT_DAY
summary(data.train$PAYMENT_DAY)
for (i in 1:dim(data.train)[1]) {
  if (data.train[i,'PAYMENT_DAY'] <= 20) {
    data.train[i,'PAYMENT_DAY'] = 1
  }
  else {
    data.train[i,'PAYMENT_DAY'] = 0
  }
}
unique(data.train$PAYMENT_DAY)

# Manipulation AGE, MONTHS_IN_RESIDENCE, and MONTHS_IN_THE_JOB
summary(data.train$AGE)
summary(data.train$MONTHS_IN_RESIDENCE/12)
data.train$MONTHS_IN_RESIDENCE = data.train$MONTHS_IN_RESIDENCE/12 # In year
colnames(data.train)[colnames(data.train) == 'MONTHS_IN_RESIDENCE'] = 'YEAR_IN_RESIDENCE'
summary(data.train$MONTHS_IN_THE_JOB/12)
data.train$MONTHS_IN_THE_JOB = data.train$MONTHS_IN_THE_JOB/12 # In year
colnames(data.train)[colnames(data.train) == 'MONTHS_IN_THE_JOB'] = 'YEAR_IN_THE_JOB'

# Case 1 -  YEAR_IN_RESIDENCE bigger than AGE
index.case.1 = which(data.train$AGE <= data.train$YEAR_IN_RESIDENCE)
data.case.1 = data.train[index.case.1,]
View(data.case.1)
summary(data.case.1$AGE)
summary(data.case.1$YEAR_IN_RESIDENCE)
for (i in 1:dim(data.train)[1]) { # Manipulation
  if (data.train[i,'AGE'] <= data.train[i,'YEAR_IN_RESIDENCE']) {
    data.train[i,'YEAR_IN_RESIDENCE'] = data.train[i,'AGE']
  }
  else {
    data.train[i,'YEAR_IN_RESIDENCE'] = data.train[i,'YEAR_IN_RESIDENCE']
  }
}

# Case 2 -  YEAR_IN_THE_JOB bigger than AGE
index.case.2 = which(data.train$AGE <= data.train$YEAR_IN_THE_JOB)
data.case.2 = data.train[index.case.2,]
View(data.case.2)
summary(data.case.1$AGE)
summary(data.case.1$YEAR_IN_RESIDENCE)
data.case.2.normal = data.train[-index.case.2,]
View(data.case.2.normal)
plot(data.case.2.normal$AGE, # Find some interest pattern between AGE and YEAR_IN_THE_JOB
     data.case.2.normal$YEAR_IN_THE_JOB)
cor(data.case.2.normal$AGE, # Correlation between AGE and YEAR_IN_THE_JOB
    data.case.2.normal$YEAR_IN_THE_JOB)
for (i in 1:dim(data.train)[1]) { # Manipulation
  if (data.train[i,'AGE'] <= data.train[i,'YEAR_IN_THE_JOB']) {
    data.train[i,'YEAR_IN_THE_JOB'] = data.train[i,'AGE'] - 15
  }
  else {
    data.train[i,'YEAR_IN_THE_JOB'] = data.train[i,'YEAR_IN_THE_JOB']
  }
}

# Drop FLAG_OTHER_CARD, FLAG_MOBILE_PHONE, and FLAG_CONTACT_PHONE
data.train = data.train[,-c(16,19,20)]

# SEX and MATE_INCOME
index.single = which(data.train$MARITAL_STATUS == 'S' & data.train$MATE_INCOME > 0)
data.train = data.train[-index.single,]

# PERSONAL NET INCOME
summary(data.train$PERSONAL_NET_INCOME)
index.per.net.income = order(data.train$PERSONAL_NET_INCOME,
                             decreasing = TRUE)
index.per.net.income = index.per.net.income[1:16]
data.train = data.train[-index.per.net.income,]
View(data.train)

# ===== MANIPULATION VALUES EACH VARIABLES
data.train.mod = data.train
# SEX
data.train.mod[which(data.train.mod$SEX == 'F'),'SEX'] = 0
data.train.mod[which(data.train.mod$SEX == 'M'),'SEX'] = 1
# MARITAL_STATUS
marital.status = data.train.mod[,c(1,2)]
# Single
marital.status.s = rep(x = 0,
                       len = dim(data.train.mod)[1])
for (i in 1:dim(data.train.mod)[1]) {
  if (data.train.mod[i,'MARITAL_STATUS'] == 'S') {
    marital.status.s[i] = 1
  }
  else {
    marital.status.s[i] = 0
  }
}

View(data.train.mod)
str(data.train)
write.csv(data.train,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean FIX.csv',
          row.names = FALSE)
