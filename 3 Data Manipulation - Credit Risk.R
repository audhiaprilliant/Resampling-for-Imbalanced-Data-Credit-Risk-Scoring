# ===== MANIPULATION DATASET - DATA MINING =====

data.man = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean FIX.csv',
                    header = TRUE,
                    sep = ',')
View(data.man)
str(data.man)

# MANIPULATION SEX
data.man$SEX = as.character(data.man$SEX)
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'SEX'] == 'M') {
    data.man[i,'SEX'] = 1
  }
  if (data.man[i,'SEX'] == 'F') {
    data.man[i,'SEX'] = 0
  }
}

# MANIPULATION MARITAL_STATUS
data.man$MARITAL_STATUS = as.character(data.man$MARITAL_STATUS)
unique(data.man$MARITAL_STATUS)
# Single
MARITAL_STATUS_S = 0
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'MARITAL_STATUS'] == 'S') {
    MARITAL_STATUS_S[i] = 1
  }
  else {
    MARITAL_STATUS_S[i] = 0
  }
}

# Married
MARITAL_STATUS_C = 0
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'MARITAL_STATUS'] == 'C') {
    MARITAL_STATUS_C[i] = 1
  }
  else {
    MARITAL_STATUS_C[i] = 0
  }
}
# Other
MARITAL_STATUS_O = 0
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'MARITAL_STATUS'] == 'O') {
    MARITAL_STATUS_O[i] = 1
  }
  else {
    MARITAL_STATUS_O[i] = 0
  }
}
# Widow
MARITAL_STATUS_V = 0
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'MARITAL_STATUS'] == 'V') {
    MARITAL_STATUS_V[i] = 1
  }
  else {
    MARITAL_STATUS_V[i] = 0
  }
}
# Divorced - Not used
MARITAL_STATUS_D = 0
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'MARITAL_STATUS'] == 'D') {
    MARITAL_STATUS_D[i] = 1
  }
  else {
    MARITAL_STATUS_D[i] = 0
  }
}

data.man.new = data.frame(SEX = data.man$SEX,
                          MARITAL_STATUS_S,
                          MARITAL_STATUS_C,
                          MARITAL_STATUS_O,
                          MARITAL_STATUS_V,
                          data.man[,3:20])
View(data.man.new)
data.man = data.man.new

# MANIPULATION FLAG_RESIDENCIAL_PHONE
data.man$FLAG_RESIDENCIAL_PHONE = as.character(data.man$FLAG_RESIDENCIAL_PHONE)
unique(data.man$FLAG_RESIDENCIAL_PHONE)
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'FLAG_RESIDENCIAL_PHONE'] == 'Y') {
    data.man[i,'FLAG_RESIDENCIAL_PHONE'] = 1
  }
  if (data.man[i,'FLAG_RESIDENCIAL_PHONE'] == 'N') {
    data.man[i,'FLAG_RESIDENCIAL_PHONE'] = 0
  }
}

# MANIPULATION RESIDENCE_TYPE
data.man$RESIDENCE_TYPE = as.character(data.man$RESIDENCE_TYPE)
unique(data.man$RESIDENCE_TYPE)
# Owned
RESIDENCE_TYPE_P = 0
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'RESIDENCE_TYPE'] == 'P') {
    RESIDENCE_TYPE_P[i] = 1
  }
  else {
    RESIDENCE_TYPE_P[i] = 0
  }
}
# Other
RESIDENCE_TYPE_O = 0
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'RESIDENCE_TYPE'] == 'O') {
    RESIDENCE_TYPE_O[i] = 1
  }
  else {
    RESIDENCE_TYPE_O[i] = 0
  }
}
# Rented
RESIDENCE_TYPE_A = 0
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'RESIDENCE_TYPE'] == 'A') {
    RESIDENCE_TYPE_A[i] = 1
  }
  else {
    RESIDENCE_TYPE_A[i] = 0
  }
}
# Parent's House - Not used
RESIDENCE_TYPE_C = 0
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'RESIDENCE_TYPE'] == 'C') {
    RESIDENCE_TYPE_C[i] = 1
  }
  else {
    RESIDENCE_TYPE_C[i] = 0
  }
}

data.man.new = data.frame(data.man[1:9],
                          RESIDENCE_TYPE_P,
                          RESIDENCE_TYPE_O,
                          RESIDENCE_TYPE_A,
                          data.man[,11:23])
View(data.man.new)
data.man = data.man.new

# MANIPULATION FLAG_MOTHERS_NAME
colnames(data.man)
data.man$FLAG_MOTHERS_NAME = as.character(data.man$FLAG_MOTHERS_NAME)
unique(data.man$FLAG_MOTHERS_NAME)
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'FLAG_MOTHERS_NAME'] == 'Y') {
    data.man[i,'FLAG_MOTHERS_NAME'] = 1
  }
  if (data.man[i,'FLAG_MOTHERS_NAME'] == 'N') {
    data.man[i,'FLAG_MOTHERS_NAME'] = 0
  }
}

# MANIPULATION FLAG_FATHERS_NAME
colnames(data.man)
data.man$FLAG_FATHERS_NAME = as.character(data.man$FLAG_FATHERS_NAME)
unique(data.man$FLAG_FATHERS_NAME)
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'FLAG_FATHERS_NAME'] == 'Y') {
    data.man[i,'FLAG_FATHERS_NAME'] = 1
  }
  if (data.man[i,'FLAG_FATHERS_NAME'] == 'N') {
    data.man[i,'FLAG_FATHERS_NAME'] = 0
  }
}

# MANIPULATION FLAG_RESIDENCE_TOWN.WORKING_TOWN
colnames(data.man)
data.man$FLAG_RESIDENCE_TOWN.WORKING_TOWN = as.character(data.man$FLAG_RESIDENCE_TOWN.WORKING_TOWN)
unique(data.man$FLAG_RESIDENCE_TOWN.WORKING_TOWN)
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'FLAG_RESIDENCE_TOWN.WORKING_TOWN'] == 'Y') {
    data.man[i,'FLAG_RESIDENCE_TOWN.WORKING_TOWN'] = 1
  }
  if (data.man[i,'FLAG_RESIDENCE_TOWN.WORKING_TOWN'] == 'N') {
    data.man[i,'FLAG_RESIDENCE_TOWN.WORKING_TOWN'] = 0
  }
}

# MANIPULATION FLAG_RESIDENCE_STATE.WORKING_STATE
colnames(data.man)
data.man$FLAG_RESIDENCE_STATE.WORKING_STATE = as.character(data.man$FLAG_RESIDENCE_STATE.WORKING_STATE)
unique(data.man$FLAG_RESIDENCE_STATE.WORKING_STATE)
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'FLAG_RESIDENCE_STATE.WORKING_STATE'] == 'Y') {
    data.man[i,'FLAG_RESIDENCE_STATE.WORKING_STATE'] = 1
  }
  if (data.man[i,'FLAG_RESIDENCE_STATE.WORKING_STATE'] == 'N') {
    data.man[i,'FLAG_RESIDENCE_STATE.WORKING_STATE'] = 0
  }
}

# MANIPULATION FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS
colnames(data.man)
data.man$FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS = as.character(data.man$FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS)
unique(data.man$FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS)
for (i in 1:dim(data.man)[1]) {
  if (data.man[i,'FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS'] == 'Y') {
    data.man[i,'FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS'] = 1
  }
  if (data.man[i,'FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS'] == 'N') {
    data.man[i,'FLAG_RESIDENCIAL_ADDRESS.POSTAL_ADDRESS'] = 0
  }
}

# SET AS FACTOR
colnames(data.man)
str(data.man)
for (i in c(1:5,7:12,14:17,20:22,24,25)) {
  data.man[,i] = as.factor(data.man[,i])
}

write.csv(x = data.man,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Data Mining/Final Assignment/Datasets/Data Train Clean Manipulation.csv',
          row.names = FALSE)


index.net.income = order(data.train$PERSONAL_NET_INCOME,
                         decreasing = FALSE)
View(data.train[index.net.income,])
index.error = which(data.man$MATE_INCOME == 0 & data.man$PERSONAL_NET_INCOME == 0 & data.man$TARGET_LABEL_BAD.1 == 1)
View(data.man[index.error,])
