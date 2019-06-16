# ===== RE-SAMPLING METHODS =====

# ===== READ DATA
data.train = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean Manipulation.csv',
                      header = TRUE,
                      sep = ',')
View(data.train)
str(data.train)
summary(data.train)
colnames(data.train)

# ===== CHANGE VARIABLES'S TYPES
for (i in c(1:5,7:12,14:17,20:22,24,25)) {
  data.train[,i] = as.factor(data.train[,i])
}

# IMPORTANT VARIABLES BASED ON DECISION TREE
data.train = data.train[,c(1,2,3,6,7,8,10,12,13,16,18,22,23,24,25)]

# ===== SMOTE
library(DMwR)
library(ROSE)
data.smote = SMOTE(TARGET_LABEL_BAD.1~.,
                   data = data.train)
View(data.smote)
str(data.smote)
prop.table(table(data.smote$TARGET_LABEL_BAD.1))
write.csv(x = data.smote,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean SMOTE.csv',
          row.names = FALSE)

# ===== ROSE SAMPLING
data.rose = ROSE(TARGET_LABEL_BAD.1~.,
                 data = data.train,
                 p = 0.5,
                 seed = 1)$data
prop.table(table(data.rose$TARGET_LABEL_BAD.1))
str(data.rose)
write.csv(x = data.rose,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean ROSE.csv',
          row.names = FALSE)

# ===== OVER SAMPLING
data.os = ovun.sample(TARGET_LABEL_BAD.1~.,
                      data = data.train,
                      p = 0.5,
                      seed = 1,
                      method = 'over')$data
prop.table(table(data.os$TARGET_LABEL_BAD.1))
str(data.os)
write.csv(x = data.os,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean OS.csv',
          row.names = FALSE)

# ===== UNDER SAMPLING
data.us = ovun.sample(TARGET_LABEL_BAD.1~.,
                      data = data.train,
                      p = 0.5,
                      seed = 1,
                      method = 'under')$data
prop.table(table(data.us$TARGET_LABEL_BAD.1))
str(data.us)
write.csv(x = data.us,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean US.csv',
          row.names = FALSE)