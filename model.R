
# Importing required libraries
library(randomForest) #imports randomForest function
library(caret) #imports Caret, this is the package responsible for spliting and training data

# Loading the dataset 
sizeData <- read.csv("sizeFile.csv")

# Training and splitting the data
TrainingIndex <- createDataPartition(sizeData$Size, p=0.8, list = FALSE)
TrainingSize <- sizeData[TrainingIndex,] 
TestingSize <- sizeData[-TrainingIndex,] 

# Creating dummy files to store the trained and testing data
# Doing it this way allows the app to be able to continue from the previous state
# Also helps to manage the file sizes and reduces clutter in cases when the training and testing data gets very large
write.csv(TrainingSize, "trainingSize.csv")
write.csv(TestingSize, "testingSize.csv")

# Enables the header labels for the dataset
TrainSize <- read.csv("trainingSize.csv", header = TRUE)
TrainSize <- TrainSize[,-1]

# Invokes the randomForest function
# Builds the model that will be used for the prediction
sizeModel <- randomForest(as.factor(Size) ~ ., data = TrainSize, ntree = 500, mtry = 4, importance = TRUE)

# Generates the RDS file the will be used for the prediction
saveRDS(sizeModel, "sizeModel.rds")
