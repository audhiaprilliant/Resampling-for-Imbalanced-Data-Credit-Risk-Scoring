# ===== CLASSIFICATION - DECISION TREE =====

library(caret)
library(rattle)
library(DMwR)
library(ROSE)

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
dec.tree = train(TARGET_LABEL_BAD.1~.,
                 data = train,
                 method = 'rpart',
                 metric = 'ROC',
                 trControl = x,
                 tuneLength = tunel)
dec.tree
plot(dec.tree)
varImp(dec.tree)
# MAKING PREDICTION
valid_pred = predict(dec.tree, train)
#STORING MODEL PERFORMANCE SCORES
confusionMatrix(valid_pred, train$TARGET_LABEL_BAD.1)
fancyRpartPlot(dec.tree$finalModel)