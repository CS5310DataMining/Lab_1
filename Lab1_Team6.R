#Lab: 1
#Team: 6
#Team Members:
  #Melvin Zaldivar - Members contribution: 33.33%
  #Rahim Abdulmalik - Members contribution: 33.33%
  #Raul Beiza - Members contribution: 33.33%

# Due Date: February 2, 2020

## k-NN Lazy Learning - Classfication Using 
#  Nearest Neighbor

#==================================================
# Step-2: Explore and prepare the data
#==================================================

# Import the data
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# Read the data
data.frame(wbcd)

# Remove the ID feature
wbcd <- wbcd[-1]

# Obtain outcome of the Diagnosis and create table
table(wbcd$diagnosis)

# Obtain percentage for each diagnose case
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)

# Changing diagnosis variable to a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), 
                         labels = c("Benign","Malignant"))

# Summary information for three feature
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

# Normalizing numeric data
normalize <- function(x) {
  return ((x - min(x))/(max(x)-min(x)))
}

# Verifying the normalization function works
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

# Apply the normalize function to columns 2 through 31
wbcd_n <-as.data.frame(lapply(wbcd[2:31],normalize))                  

# Confrim the transformation was applied correctly
summary(wbcd_n[c("radius_mean","area_mean","smoothness_mean")])

# Data Preparation - Creating training and test datasets
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

# Creating labls for the class 
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

#================================================
# Step-3: Train a model on the data
#================================================

# Install the "class" package for k-NN
install.packages("class")

# Load the "class package for k-NN
library(class)

wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=21)

#================================================
# Step-4: Evaluate the model performance
#================================================

# Installing gmodels to perform evaluation on train and test data
install.packages("gmodels")

# loading gmodels 
library(gmodels)

# Evaluting how well the predicted classes match the
# actual values 

CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)

#================================================
# Step-5: Improving the model performace
#===============================================

# Rescaling the values using the z-score standardization
wbcd_z <-as.data.frame(scale(wbcd[-1]))

# Repeating steps 2.1- to Step-4
wbcd_train_z <-wbcd_z[1:469,]
wbcd_test_z <- wbcd_z[470:569,]
wbcd_train_labels_z <- wbcd[1:469,1]
wbcd_test_labels_z <- wbcd[470:569,1]
wbcd_test_pred_z <- knn(train=wbcd_train_z, test = wbcd_test_z,
                      cl = wbcd_train_labels_z, k=21)

CrossTable(x = wbcd_test_labels_z, y = wbcd_test_pred_z,
           prop.chisq = FALSE)

#===============================================
# Repeating for alternative values of k

wbcd_train_k <- wbcd_n[1:469,]
wbcd_test_k <- wbcd_n[470:569,]

# Using k value of 1
wbcd_test_pred1 <- knn(train = wbcd_train_k, test = wbcd_test_k ,
                      cl=wbcd_train_labels, k=1 )
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred1, prop.chisq = FALSE)

# Using k value of 5
wbcd_test_pred5 <- knn(train = wbcd_train_k, test = wbcd_test_k ,
                       cl=wbcd_train_labels, k=5 )
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred5, prop.chisq = FALSE)

# Using k value of 11
wbcd_test_pred11 <- knn(train = wbcd_train_k, test = wbcd_test_k ,
                       cl=wbcd_train_labels, k=11 )
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred11, prop.chisq = FALSE)

# Using k value of 15
wbcd_test_pred15 <- knn(train = wbcd_train_k, test = wbcd_test_k ,
                       cl=wbcd_train_labels, k=15 )
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred15, prop.chisq = FALSE)

# Using k value of 24
wbcd_test_pred24 <- knn(train = wbcd_train_k, test = wbcd_test_k ,
                       cl=wbcd_train_labels, k=24 )
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred24, prop.chisq = FALSE)


