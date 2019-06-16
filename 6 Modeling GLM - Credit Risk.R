# ===== CLASSIFICATION - SUPPORT VECTOR MACHINE =====

library(caret)
library(rattle)
library(DMwR)
library(ROSE)

# ===== READ DATA
data.train = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean SMOTE.csv',
                      header = TRUE,
                      sep = ',')
table(data.train$TARGET_LABEL_BAD.1)
table(train$TARGET_LABEL_BAD.1)
table(validation$TARGET_LABEL_BAD.1)
View(data.train)
str(data.train)
summary(data.train)
colnames(data.train)

# ===== CHANGE VARIABLES'S TYPES
for (i in c(1:5,7:12,14:17,20:22,24,25)) {
  data.train[,i] = as.factor(data.train[,i])
}
data.train$TARGET_LABEL_BAD.1 = as.factor(data.train$TARGET_LABEL_BAD.1)
data.train = data.train[,-c(9,14,17,21,24)]

# ===== DATA PARTITIONING
set.seed(100)
index = createDataPartition(data.train$TARGET_LABEL_BAD.1, p = 0.7, list = FALSE)
train = data.train[index,]
validation = data.train[-index,]
# EXPLORE DATA
dim(train)
dim(validation)
head(train)
head(validation)
# SETTING LEVELS FOR BOTH TRAINING AND VALIDATION DATA
levels(train$TARGET_LABEL_BAD.1) = make.names(levels(factor(train$TARGET_LABEL_BAD.1)))
levels(validation$TARGET_LABEL_BAD.1) = make.names(levels(factor(validation$TARGET_LABEL_BAD.1)))

# ===== DECISION TREE CLASSIFICATION =====
# SETTING UP TRAIN CONTROLS
repeats = 3
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = 'repeatedcv',
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)
glm.model = train(TARGET_LABEL_BAD.1~.,
                  data = train,
                  method = 'glm',
                  metric = 'ROC',
                  trControl = x,
                  tuneLength = tunel)
glm.model
ggplot(glm.model)+
  theme_bw()+
  labs(title = 'Data Credit Risk - ROSE')
ggplot(varImp(glm.model))+
  labs(title = 'Important Variables',
       subtitle = 'Credit Risk Scoring')+
  theme_bw()
# MAKING PREDICTION
valid_pred = predict(glm.model, validation)
#STORING MODEL PERFORMANCE SCORES
confusionMatrix(valid_pred, validation$TARGET_LABEL_BAD.1)
fancyRpartPlot(dec.tree$finalModel)