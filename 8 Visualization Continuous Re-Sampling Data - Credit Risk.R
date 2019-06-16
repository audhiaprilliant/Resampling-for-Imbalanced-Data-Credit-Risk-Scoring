# ===== CLASSIFICATION - DECISION TREE TUNNING PARAMETER =====

library(ggplot2)

# ===== READ DATA
data.original = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean Manipulation.csv',
                         header = TRUE,
                         sep = ',')
data.os = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean OS.csv',
                   header = TRUE,
                   sep = ',')
data.us = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean US.csv',
                   header = TRUE,
                   sep = ',')
data.smote = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean SMOTE.csv',
                      header = TRUE,
                      sep = ',')
data.rose = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean ROSE.csv',
                     header = TRUE,
                     sep = ',')

# ===== DATA ORIGINAL
data.continuous.original = data.frame(AGE = data.original$AGE,
                                      YEAR_IN_RESIDENCE = data.original$YEAR_IN_RESIDENCE,
                                      YEAR_IN_THE_JOB = data.original$YEAR_IN_THE_JOB,
                                      MATE_INCOME = data.original$MATE_INCOME,
                                      PERSONAL_NET_INCOME = data.original$PERSONAL_NET_INCOME,
                                      Label = 'Original')
# ===== DATA OVER SAMPLING
data.continuous.os = data.frame(AGE = data.os$AGE,
                                YEAR_IN_RESIDENCE = data.os$YEAR_IN_RESIDENCE,
                                YEAR_IN_THE_JOB = data.os$YEAR_IN_THE_JOB,
                                MATE_INCOME = data.os$MATE_INCOME,
                                PERSONAL_NET_INCOME = data.os$PERSONAL_NET_INCOME,
                                Label = 'Over Sampling')
# ===== DATA UNDER SAMPLING
data.continuous.us = data.frame(AGE = data.us$AGE,
                                YEAR_IN_RESIDENCE = data.us$YEAR_IN_RESIDENCE,
                                YEAR_IN_THE_JOB = data.us$YEAR_IN_THE_JOB,
                                MATE_INCOME = data.us$MATE_INCOME,
                                PERSONAL_NET_INCOME = data.us$PERSONAL_NET_INCOME,
                                Label = 'Under Sampling')
# ===== DATA SMOTE
data.continuous.smote = data.frame(AGE = data.smote$AGE,
                                   YEAR_IN_RESIDENCE = data.smote$YEAR_IN_RESIDENCE,
                                   YEAR_IN_THE_JOB = data.smote$YEAR_IN_THE_JOB,
                                   MATE_INCOME = data.smote$MATE_INCOME,
                                   PERSONAL_NET_INCOME = data.smote$PERSONAL_NET_INCOME,
                                   Label = 'SMOTE')
# ===== DATA ROSE
data.continuous.rose = data.frame(AGE = data.rose$AGE,
                                  YEAR_IN_RESIDENCE = data.rose$YEAR_IN_RESIDENCE,
                                  YEAR_IN_THE_JOB = data.rose$YEAR_IN_THE_JOB,
                                  MATE_INCOME = data.rose$MATE_INCOME,
                                  PERSONAL_NET_INCOME = data.rose$PERSONAL_NET_INCOME,
                                  Label = 'ROSE')

# ===== COMBINE DATA
data.continuous.all = data.frame(rbind(data.continuous.original,
                                       data.continuous.os,
                                       data.continuous.us,
                                       data.continuous.smote,
                                       data.continuous.rose))

View(data.continuous.all)

# ===== VISUALIZATION
ggplot(data.continuous.all)+
  geom_density(aes(AGE,
                   fill = Label),
               alpha = 0.8)+
  labs(title = 'Density Plot of Age',
       subtitle = 'Credit Scoring')+
  xlab('Age')+
  ylab('Density')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggplot(data.continuous.all)+
  geom_density(aes(YEAR_IN_RESIDENCE,
                   fill = Label),
               alpha = 0.8)+
  labs(title = 'Density Plot of Age',
       subtitle = 'Credit Scoring')+
  xlab('Year in Residence')+
  ylab('Density')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggplot(data.continuous.all)+
  geom_density(aes(YEAR_IN_THE_JOB,
                   fill = Label),
               alpha = 0.8)+
  labs(title = 'Density Plot of Age',
       subtitle = 'Credit Scoring')+
  xlab('Year in the Job')+
  ylab('Density')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggplot(data.continuous.all)+
  geom_density(aes(MATE_INCOME,
                   fill = Label),
               alpha = 0.8)+
  labs(title = 'Density Plot of Age',
       subtitle = 'Credit Scoring')+
  xlab('Mate Income')+
  ylab('Density')+
  theme_bw()+
  theme(legend.position = 'bottom')
