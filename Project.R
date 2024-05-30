# Import library
library(tidyverse)
library(caret)
library(ellipse)
library(pROC)

# Import diabetes data set
diab <- read.csv(file = 'diabetic_data.csv', header = TRUE, sep = ',')

# Check for missing values and the percentage
na <- diab == '?'
is.na(diab) <- na
colMeans(is.na(diab))*100

# Remove features with approximately or more than 40% missing values
diab$weight <- NULL
diab$payer_code <- NULL
diab$medical_specialty <- NULL

# Remove 'encounter_id' feature because some patients had multiple visits
diab$encounter_id <- NULL

# Remove patients with multiple visits and remove 'patient_nbr' feature
diab <- diab[!duplicated(diab$patient_nbr), ]
diab$patient_nbr <- NULL

# Remove 'race' feature as it does not affect the rate of readmission
diab$race <- NULL

# Check features with near-zero variance
nearZeroVar(diab, saveMetrics = TRUE)

# Remove zero-variance and near-zero variance features
diab$max_glu_serum <- NULL
diab$repaglinide <- NULL
diab$nateglinide <- NULL
diab$chlorpropamide <- NULL
diab$glimepiride <- NULL
diab$acetohexamide <- NULL
diab$tolbutamide <- NULL
diab$acarbose <- NULL
diab$miglitol <- NULL
diab$troglitazone <- NULL
diab$tolazamide <- NULL
diab$examide <- NULL
diab$citoglipton <- NULL
diab$glyburide.metformin <- NULL
diab$glipizide.metformin <- NULL
diab$glimepiride.pioglitazone <- NULL
diab$metformin.rosiglitazone <- NULL
diab$metformin.pioglitazone <- NULL

# Omit rows with NA value
diab = na.omit(diab)
colSums(is.na(diab))

# Omit rows containing 'Unknown/Invalid' in 'gender' feature
diab = diab[!grepl('Unknown/Invalid', diab$gender), ]

# Categorize feature 'admission_type_id'
diab$admission_type_id <- replace(diab$admission_type_id, diab$admission_type_id == 2, 1)
diab$admission_type_id <- replace(diab$admission_type_id, diab$admission_type_id == 7, 1)
diab$admission_type_id <- replace(diab$admission_type_id, diab$admission_type_id == 6, 5)
diab$admission_type_id <- replace(diab$admission_type_id, diab$admission_type_id == 8, 5)
diab$admission_type_id <- str_replace(diab$admission_type_id, '1', 'Emergency')
diab$admission_type_id <- str_replace(diab$admission_type_id, '3', 'Elective')
diab$admission_type_id <- str_replace(diab$admission_type_id, '4', 'Newborn')
diab$admission_type_id <- str_replace(diab$admission_type_id, '5', 'Other')
unique(diab$admission_type_id)
table(diab$admission_type_id)

# Categorize feature 'discharge_disposition_id' according to the values
# Remove ids containing value 11, 19, 20, 21
diab <- diab[!diab$discharge_disposition_id %in% c(11, 19, 20, 21), ]
diab$discharge_disposition_id <- case_when(diab$discharge_disposition_id %in% c('7', '9', '12') ~ 'Other', 
                                           diab$discharge_disposition_id %in% c('18', '25', '26') ~ 'Unknown', 
                                           diab$discharge_disposition_id %in% c('13', '14') ~ 'Hospice', 
                                           TRUE ~ 'Discharged')
unique(diab$discharge_disposition_id)
table(diab$discharge_disposition_id)

# Categorize 'admission_source_id' accordingly
diab$admission_source_id <- case_when(diab$admission_source_id %in% c('1', '2', '3') ~ 'Referral', 
                                      diab$admission_source_id %in% c('4', '5', '6', '10', '18', '22', '25', '26') ~ 'Transfer', 
                                      diab$admission_source_id %in% c('7') ~ 'Emergency Room', 
                                      TRUE ~ 'Other')
unique(diab$admission_source_id)
table(diab$admission_source_id)

# Categorize feature 'diag_1' according to the icd9 codes
diab <- mutate(diab, diag_1 =
                 ifelse(str_detect(diag_1, 'E') | str_detect(diag_1, 'V'), 'Other', 
                        ifelse(str_detect(diag_1, '250'), 'Diabetes',
                               ifelse( as.integer(diag_1) == 785 | (as.integer(diag_1) <= 459 & as.integer(diag_1) >= 390), 'Circulatory',
                                      ifelse( as.integer(diag_1) == 786 | (as.integer(diag_1) <= 519 & as.integer(diag_1) >= 460), 'Respiratory', 
                                             ifelse( as.integer(diag_1) == 787 | (as.integer(diag_1) <= 579 & as.integer(diag_1) >= 520), 'Digestive', 
                                                    ifelse( as.integer(diag_1) == 788 | (as.integer(diag_1) <= 629 & as.integer(diag_1)  >= 580), 'Genitourinary',
                                                           ifelse((as.integer(diag_1) <= 239 & as.integer(diag_1) >= 140), 'Neoplasms',  
                                                                  ifelse((as.integer(diag_1) <= 739 & as.integer(diag_1) >= 710), 'Musculoskeletal',          
                                                                         ifelse((as.integer(diag_1) <= 999 & as.integer(diag_1) >= 800), 'Injury',                    
                                                                                'Other'))))))))))
unique(diab$diag_1)
table(diab$diag_1)

# Categorize feature 'diag_2' according to the icd9 codes
diab <- mutate(diab, diag_2 =
                 ifelse(str_detect(diag_2, 'E') | str_detect(diag_2, 'V'), 'Other', 
                        ifelse(str_detect(diag_2, '250'), 'Diabetes',
                               ifelse(as.integer(diag_2) == 785 | (as.integer(diag_2) <= 459 & as.integer(diag_2) >= 390), 'Circulatory',
                                      ifelse(as.integer(diag_2) == 786 | (as.integer(diag_2) <= 519 & as.integer(diag_2) >= 460), 'Respiratory', 
                                             ifelse(as.integer(diag_2) == 787 | (as.integer(diag_2) <= 579 & as.integer(diag_2) >= 520), 'Digestive', 
                                                    ifelse( as.integer(diag_2) == 788 | (as.integer(diag_2) <= 629 & as.integer(diag_2)  >= 580), 'Genitourinary',
                                                           ifelse((as.integer(diag_2) <= 239 & as.integer(diag_2) >= 140), 'Neoplasms',  
                                                                  ifelse((as.integer(diag_2) <= 739 & as.integer(diag_2) >= 710), 'Musculoskeletal',          
                                                                         ifelse((as.integer(diag_2) <= 999 & as.integer(diag_2) >= 800), 'Injury',                    
                                                                                'Other'))))))))))
unique(diab$diag_2)
table(diab$diag_2)

# Categorize feature 'diag_3' according to the icd9 codes
diab <- mutate(diab, diag_3 =
                 ifelse(str_detect(diag_3, 'E') | str_detect(diag_3, 'V'), 'Other', 
                        ifelse(str_detect(diag_3, '250'), 'Diabetes',
                               ifelse(as.integer(diag_3) == 785 | (as.integer(diag_3) <= 459 & as.integer(diag_3) >= 390), 'Circulatory',
                                      ifelse(as.integer(diag_3) == 786 | (as.integer(diag_3) <= 519 & as.integer(diag_3) >= 460), 'Respiratory', 
                                             ifelse(as.integer(diag_3) == 787 | (as.integer(diag_3) <= 579 & as.integer(diag_3) >= 520), 'Digestive', 
                                                    ifelse( as.integer(diag_3) == 788 | (as.integer(diag_3) <= 629 & as.integer(diag_3)  >= 580), 'Genitourinary',
                                                            ifelse((as.integer(diag_3) <= 239 & as.integer(diag_3) >= 140), 'Neoplasms',  
                                                                   ifelse((as.integer(diag_3) <= 739 & as.integer(diag_3) >= 710), 'Musculoskeletal',          
                                                                          ifelse((as.integer(diag_3) <= 999 & as.integer(diag_3) >= 800), 'Injury',                    
                                                                                 'Other'))))))))))
unique(diab$diag_3)
table(diab$diag_3)

# Categorize 'readmitted' to 1 for patient readmitted within 30 days and 
# 0 for no readmission or readmitted after 30 days
diab$readmitted <- case_when(diab$readmitted %in% c('>30', 'NO') ~ '0', TRUE ~ '1')
unique(diab$readmitted)
table(diab$readmitted)

# Factorize features
str(diab)
diab$gender <- as.factor(diab$gender)
diab$age <- as.factor(diab$age)
diab$admission_type_id <- as.factor(diab$admission_type_id)
diab$discharge_disposition_id <- as.factor(diab$discharge_disposition_id)
diab$admission_source_id <- as.factor(diab$admission_source_id)
diab$diag_1 <- as.factor(diab$diag_1)
diab$diag_2 <- as.factor(diab$diag_2)
diab$diag_3 <- as.factor(diab$diag_3)
diab$A1Cresult <- as.factor(diab$A1Cresult)
diab$metformin <- as.factor(diab$metformin)
diab$glipizide <- as.factor(diab$glipizide)
diab$glyburide <- as.factor(diab$glyburide)
diab$pioglitazone <- as.factor(diab$pioglitazone)
diab$rosiglitazone <- as.factor(diab$rosiglitazone)
diab$insulin <- as.factor(diab$insulin)
diab$change <- as.factor(diab$change)
diab$diabetesMed <- as.factor(diab$diabetesMed)
diab$readmitted <- as.factor(diab$readmitted)
str(diab)

# Checking for outliers
par(mfrow = c(2,4))
boxplot(diab$time_in_hospital, main = 'TIH')
boxplot(diab$num_lab_procedures, main = 'NLP')
boxplot(diab$num_procedures, main = 'NP')
boxplot(diab$num_medications, main = 'NM')
boxplot(diab$number_outpatient, main = 'NO')
boxplot(diab$number_emergency, main = 'NE')
boxplot(diab$number_inpatient, main = 'NI')
boxplot(diab$number_diagnoses, main = 'ND')

# Remove features 'number_outpatient', 'number_emergency', and 'number_impatient' 
# because of outliers
diab$number_outpatient <- NULL
diab$number_emergency <- NULL
diab$number_inpatient <- NULL

# Remove outliers from other numerical features
remove_outliers <- function(rm_outliers){
  dataframe <- rm_outliers
  b <- c()
  count <- 1
  for(i in 1:ncol(dataframe)){
    if(is.integer(dataframe[,i])){
      Quartile3 <- quantile(dataframe[,i], .75, na.rm = TRUE)
      Quartile1 <- quantile(dataframe[,i], .25, na.rm = TRUE) 
      InterquartileRange <- Quartile3 - Quartile1
      upbound <- Quartile3 + 1.5 * InterquartileRange
      lowbound <- Quartile1 - 1.5 * InterquartileRange
      for(j in 1:nrow(dataframe)){
        if(is.na(dataframe[j,i]) == TRUE){
          next
        }
        else if(dataframe[j,i] > upbound | dataframe[j,i] < lowbound){
          b[count] <- j
          count <- count+1                  
        }
      }
    }
  }
  dataframe <- dataframe[-b,]
}
diab <- remove_outliers(diab)

# Output the processed data file
write.csv(diab, file = 'processed_diabetes.csv')

# Supervised Machine Learning
# Split data into 80% training set and 20% testing set
testing_index <- createDataPartition(diab$readmitted, p = 0.80, list = FALSE)

# 20% of data is selected for testing
testing <- diab[-testing_index, ]

# Remaining 80% for training
training <- diab[testing_index, ]

# Data Balancing using ROSE
install.packages('ROSE')
library(ROSE)
balanced <- ROSE(readmitted ~., data = training)$data
table(balanced$readmitted)

# 10-fold cross-validation on dataset
control <- trainControl(method = 'cv', number = 10)
metric <- 'Accuracy'

# CART
fit.cart <- train(readmitted ~., data = balanced, method = 'rpart', metric = metric, 
                  trControl = control)
fit.cart
predictions.cart <- predict(fit.cart, testing)
confusionMatrix(predictions.cart, testing$readmitted)
roc_cart <- roc(testing$readmitted, factor(predictions.cart, ordered = TRUE), 
                plot = TRUE, main = 'ROC curve: CART')
auc(roc_cart)

# KNN
fit.knn <- train(readmitted ~., data = balanced, method = 'knn', metric = metric, 
                 trControl = control)
fit.knn
predictions.knn <- predict(fit.knn, testing)
confusionMatrix(predictions.knn, testing$readmitted)
roc_knn <- roc(testing$readmitted, factor(predictions.knn, ordered = TRUE), 
               plot = TRUE, main = 'ROC curve: KNN')
auc(roc_knn)

# Random Forest
fit.rf <- train(readmitted ~., data = balanced, method = 'rf', metric = metric, 
                trControl = control)
fit.rf
predictions.rf <- predict(fit.rf, testing)
confusionMatrix(predictions.rf, testing$readmitted)
roc_rf <- roc(testing$readmitted, factor(predictions.rf, ordered = TRUE), 
              plot = TRUE, main = 'ROC curve: RF')
auc(roc_rf)

# Check results
results <- resamples(list(cart = fit.cart, knn = fit.knn, rf = fit.rf))
summary(results)
dotplot(results)
