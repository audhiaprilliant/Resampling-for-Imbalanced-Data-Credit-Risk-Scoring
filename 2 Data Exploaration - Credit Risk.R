# ===== DATA EXPLORATION - DATA MINING =====

library(corrplot)
library(ggplot2)
library(mice)
library(grid)
library(car)

# ===== READ DATA
data.train = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean FIX.csv',
                              header = TRUE,
                              sep = ',')
View(data.train)
colnames(data.train)
str(data.train)
summary(data.train)
for (i in c(1,2,4:7,9:12,15:17,20)) {
  data.train[,i] = as.factor(data.train[,i])
}

# ===== DATA VISUALIZATION
# Barplot of Sex
data.sex = data.frame(table(data.train$SEX))
ggplot(data.sex)+
  geom_bar(aes(x = Var1,
               y = Freq),
           fill = 'blue',
           col = 'white',
           alpha = 0.7,
           stat = 'identity')+
  xlab('Sex')+
  ylab('Frequency')+
  labs(title = 'Barplot of Sex',
       subtitle = 'Credit Scoring')
# Barplot Sex each Target Labels
data.sex.target = data.frame(table(data.train$SEX,
                                   data.train$TARGET_LABEL_BAD.1))
names(data.sex.target) = c('Sex',
                           'Label',
                           'Count')
ggplot(data = data.sex.target,
       aes(x = Sex,
           y = Count,
           fill = Label))+
  geom_bar(stat = 'identity',
           position = 'dodge')+
  xlab('Sex')+
  ylab('Frequency')+
  labs(title = 'Barplot of Sex',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels=c('Female',
                            'Male'))+
  theme_bw()

# Barplot of Marital Status
marital.stat = data.frame(table(data.train$MARITAL_STATUS))
ggplot(marital.stat)+
  geom_bar(aes(x = reorder(Var1,
                           -Freq),
               y = Freq),
           fill = 'red',
           col = 'white',
           alpha = 0.7,
           stat = 'identity')+
  xlab('Marital Status')+
  ylab('Frequency')+
  labs(title = 'Barplot of Martial Status',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels = c('Married',
                              'Divorced',
                              'Other',
                              'Single',
                              'Widow'))
# Barplot of Marital Status and Target Labels
data.ms.target = data.frame(table(data.train$MARITAL_STATUS,
                                  data.train$TARGET_LABEL_BAD.1))
names(data.ms.target) = c('Marital.Status',
                          'Label',
                          'Count')
ggplot(data = data.ms.target,
       aes(x = reorder(Marital.Status,
                       -Count),
           y = Count,
           fill = Label))+
  geom_bar(stat = 'identity',
           position = 'dodge')+
  xlab('Marital Status')+
  ylab('Frequency')+
  labs(title = 'Barplot of Marital Status',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels = c('Single',
                              'Married',
                              'Other',
                              'Widow',
                              'Divorced'))+
  theme_bw()

# Density Plot of Age
ggplot(data.train)+
  geom_density(aes(AGE),
               fill = 'red',
               alpha = 0.7)+
  xlab('Age of Applicants')+
  ylab('Frequency')+
  labs(title = "Density Plot of Applicant's Age",
       subtitle = 'Credit Scoring')
# Boxplot of Age and Marital Status
ggplot(data.train)+
  geom_boxplot(aes(x = reorder(MARITAL_STATUS,
                               -AGE),
                   y = AGE,
                   fill = MARITAL_STATUS),
               show.legend = FALSE)+
  xlab('Marital Status')+
  ylab('Age')+
  labs(title = 'Boxplot of Age',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels = c('Widow',
                              'Divorced',
                              'Married',
                              'Other',
                              'Single'))+
  theme_bw()
# Density of Age each Sex
ggplot(data.train)+
  geom_density(aes(AGE),
               fill = 'red',
               alpha = 0.7)+
  facet_wrap(~ SEX)+
  xlab('Age of Applicants')+
  ylab('Frequency')+
  labs(title = "Density Plot of Applicant's Age",
       subtitle = 'Credit Scoring')

# Barplot of Flag Residencial Phone
flag.residencial.phone = data.frame(table(data.train$FLAG_RESIDENCIAL_PHONE))
ggplot(flag.residencial.phone)+
  geom_bar(aes(x = Var1,
               y = Freq),
           fill = 'blue',
           col = 'white',
           alpha = 0.7,
           stat = 'identity')+
  xlab('Flag Residencial Phone')+
  ylab('Frequency')+
  labs(title = 'Barplot of Flag Residencial Phone',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels=c('No',
                            'Yes'))

# Barplot of Flag Residencial Phone
flag.residencial.phone.target = data.frame(table(data.train$FLAG_RESIDENCIAL_PHONE,
                                                 data.train$TARGET_LABEL_BAD.1))
names(flag.residencial.phone.target) = c('Resident.Phone',
                                         'Label',
                                         'Count')
ggplot(flag.residencial.phone.target)+
  geom_bar(aes(x = reorder(Resident.Phone,
                           -Count),
               y = Count,
               fill = Label),
           col = 'white',
           alpha = 1,
           stat = 'identity',
           position = 'dodge')+
  xlab('Flag Residencial Phone')+
  ylab('Frequency')+
  labs(title = 'Barplot of Flag Residencial Phone',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels = c('Yes',
                              'No'))

# Payment Day
data.test = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Training.csv',
                     header = TRUE,
                     sep = ',')
summary(data.test$PAYMENT_DAY)
data.test$TARGET_LABEL_BAD.1 = as.factor(data.test$TARGET_LABEL_BAD.1)
colnames(data.test)[colnames(data.test) == 'TARGET_LABEL_BAD.1'] = 'Label'
ggplot(data.test)+
  geom_density(aes(x = PAYMENT_DAY,
                   fill = Label),
               position = 'identity',
               alpha = 1,
               show.legend = TRUE)+
  xlab('Payment Day')+
  ylab('Frequency')+
  labs(title = "Density Plot of Applicant's Payment Day",
       subtitle = 'Credit Scoring')
# After Mnipulation Payment Day Variable
payment.day = data.frame(table(data.train$PAYMENT_DAY,
                               data.train$TARGET_LABEL_BAD.1))
colnames(payment.day) = c('Payment',
                          'Label',
                          'Count')
ggplot(payment.day)+
  geom_bar(aes(x = Payment,
               y = Count,
               fill = Payment),
           col = 'white',
           alpha = 1,
           stat = 'identity',
           position = 'dodge',
           show.legend = FALSE)+
  facet_wrap(~ Label)+
  xlab('Flag Residencial Phone')+
  ylab('Frequency')+
  labs(title = 'Barplot of Payment Day',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels = c('After 20th',
                              'Before 20th'))

# Barplot of Residence Type
residence.type = data.frame(table(data.train$RESIDENCE_TYPE))
ggplot(residence.type)+
  geom_bar(aes(x = reorder(Var1,
                           -Freq),
               y = Freq),
           fill = 'blue',
           col = 'white',
           alpha = 0.7,
           stat = 'identity')+
  xlab('Residence Type')+
  ylab('Frequency')+
  labs(title = 'Barplot of Residence Type',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels=c('Owned',
                            "Rented",
                            "Parent's House",
                            'Other'))+
  theme_bw()
# Barplot of Residence Type based on Labels
residence.type.label = data.frame(table(data.train$RESIDENCE_TYPE,
                                        data.train$TARGET_LABEL_BAD.1))
names(residence.type.label) = c('Resident.Type',
                                'Label',
                                'Count')
ggplot(residence.type.label)+
  geom_bar(aes(x = reorder(Resident.Type,
                           -Count),
               y = Count,
               fill = Label),
           col = 'white',
           alpha = 1,
           stat = 'identity',
           position = 'dodge')+
  xlab('Residence Type')+
  ylab('Frequency')+
  labs(title = 'Barplot of Residence Type',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels=c('Owned',
                            "Rented",
                            "Parent's House",
                            'Other'))+
  theme_bw()

# Density Plot of Mate Income based on Target Labels
ggplot(data.train)+
  geom_density(aes(MATE_INCOME),
               fill = 'red',
               alpha = 1)+
  xlab('Mate income of Applicants')+
  ylab('Frequency')+
  labs(title = 'Density Plot of Mate income of Applicants',
       subtitle = 'Credit Scoring')

# Density Plot Year in Residence
data.year.residence = data.train
data.year.residence$TARGET_LABEL_BAD.1 = as.factor(data.year.residence$TARGET_LABEL_BAD.1)
colnames(data.year.residence)[colnames(data.year.residence) == 'TARGET_LABEL_BAD.1'] = 'Label'
ggplot(data.year.residence)+
  geom_density(aes(x = YEAR_IN_RESIDENCE,
                   fill = Label),
               position = 'identity',
               alpha = 0.8,
               show.legend = TRUE)+
  xlab('Year in Residence')+
  ylab('Frequency')+
  labs(title = "Density Plot of Applicant's Year in Residence",
       subtitle = 'Credit Scoring')

# Density Plot Year in the Job
data.year.job = data.train
data.year.job$TARGET_LABEL_BAD.1 = as.factor(data.year.job$TARGET_LABEL_BAD.1)
colnames(data.year.job)[colnames(data.year.job) == 'TARGET_LABEL_BAD.1'] = 'Label'
ggplot(data.year.job)+
  geom_density(aes(x = YEAR_IN_THE_JOB,
                   fill = Label),
               position = 'identity',
               alpha = 0.8,
               show.legend = TRUE)+
  xlab('Year in the Job')+
  ylab('Frequency')+
  labs(title = "Density Plot of Applicant's Year in the Job",
       subtitle = 'Credit Scoring')

# Scatter Plot of Year in Residence and Year in the Job
ggplot(data.train)+
  geom_point(aes(x = YEAR_IN_RESIDENCE,
                 y = YEAR_IN_THE_JOB),
             color = 'blue',
             alpha = 0.7)+
  xlab('Year in Residence')+
  ylab('Year in the Job')+
  labs(title = 'Scatterplot Between Year in Residence 
and Year in the Job',
       subtitle = 'Credit Scoring')

# Density Plot of Personal Net Income
ggplot(data.train)+
  geom_density(aes(PERSONAL_NET_INCOME),
               fill = 'red',
               alpha = 0.7)+
  xlab('Personal Net Income')+
  ylab('Frequency')+
  labs(title = "Density Plot of Personal Net Income",
       subtitle = 'Credit Scoring')

# Residence Type and Marital Status
residence.type.marital = data.frame(table(data.train$MARITAL_STATUS,
                                          data.train$RESIDENCE_TYPE))
names(residence.type.marital) = c('Marital.Status',
                                  'Residence.Type',
                                  'Count')
str(residence.type.marital)
residence.type.marital$Residence.Type = as.character(residence.type.marital$Residence.Type)
for (i in 1:dim(residence.type.marital)[1]) {
  if (residence.type.marital[i,'Residence.Type'] == 'A') {
    residence.type.marital[i,'Residence.Type'] = 'Rented'
  }
  if (residence.type.marital[i,'Residence.Type'] == 'C') {
    residence.type.marital[i,'Residence.Type'] = "Parent's House"
  }
  if (residence.type.marital[i,'Residence.Type'] == 'O') {
    residence.type.marital[i,'Residence.Type'] = 'Other'
  }
  if (residence.type.marital[i,'Residence.Type'] == 'P') {
    residence.type.marital[i,'Residence.Type'] = 'Owned'
  }
}
residence.type.marital$Residence.Type = as.factor(residence.type.marital$Residence.Type)

ggplot(residence.type.marital)+
  geom_bar(aes(x = reorder(Marital.Status,
                           Count),
               y = Count,
               fill = Residence.Type),
           col = 'white',
           alpha = 1,
           stat = 'identity',
           position = 'dodge')+
  xlab('Residence Type')+
  ylab('Frequency')+
  labs(title = 'Barplot of Residence Type',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels = c('Divorced',
                              'Widow',
                              'Other',
                              'Married',
                              'Single'))+
  coord_flip()+
  theme(legend.position = 'bottom',
        legend.title = element_blank())

# Flag Card Insurance Option
data.label = data.frame(table(data.train$TARGET_LABEL_BAD.1))
ggplot(data.label)+
  geom_bar(aes(x = Var1,
               y = Freq),
           fill = 'magenta',
           col = 'white',
           alpha = 0.7,
           stat = 'identity')+
  xlab('Target Variable')+
  ylab('Frequency')+
  labs(title = 'Barplot of Target Variable',
       subtitle = 'Credit Scoring')+
  scale_x_discrete(labels=c('Good',
                            'Bad'))

