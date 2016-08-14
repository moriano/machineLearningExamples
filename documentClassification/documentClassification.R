library(RTextTools)

setwd("~/dev/code/codeInR/documentClassification")

raw <- read.csv("trainingExampleCleaned.csv")
colnames(raw) # Print the column names, we have two: Text and Class

# Lets split the data, 70% for training/testing, 30% for cross validation
base_data <- raw[1:nrow(raw)*0.7, ]
cross_validation_data <- raw[as.integer((nrow(raw)*0.7)+1):nrow(raw)-1, ]


# Now, it is time to create a text matrix for testing, the create_matrix function
# will also take care of removing numbers, making everything lower case and some 
# other requirements in order to try to make the test set as standard as possible
doc_matrix <- create_matrix(base_data$Text, language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms = 0.998)


# Finally, lets create the 'container' in RTextTools it is a type of object that allows
# to operate all the functions with a common interface. The relevant arguments here are
#      trainSize=Of the matrix given, specifies which part is for training
#      testSize=Of the matrix given, specifies which part is for testing
#      virgin=Specifies whether or not we have the correct answers.
container <- create_container(doc_matrix, base_data$Class, trainSize=1:as.integer(nrow(base_data)*0.7), 
                              testSize=as.integer((nrow(base_data)*0.7)+1):(nrow(raw)-1), virgin=FALSE)

# Algorithms to use, note that 'RF' and 'TREE' are a bit slower, that's why I am not 
# using them
algs <- c('SVM'
          ,'GLMNET'
          ,'MAXENT'
          #, 'RF'
          #, 'TREE'
          )

# Run the machine learning itself!
models <- train_models(container, algs)

# Perform predictions
classify <- classify_models(container, models)

# Get a quick analysis on the predictions
analytics <- create_analytics(container, classify)
summary(analytics)

# This is one of the MOST important parts of the code, now we are going to prepare the matrix for
# our cross validation data. Note that there is a new parameter here:
#     originalMatrix=doc_matrix ==> This is the previous test matrix that we created, we NEED 
#     this parameter here, because essentially what we are training is on the words, and we 
#     need to have the same words in the train, test and cross validation sets. The 
#     originalMatrix parameter takes care of that
doc_matrix_cv2 <- create_matrix(cross_validation_data$Text, 
                               originalMatrix=doc_matrix,
                               language="english", 
                               removeNumbers=TRUE,
                               stemWords=TRUE, 
                               removeSparseTerms=0.998
                               )


# Now again, lets create a container and perform predictions on it
container_cv <- create_container(doc_matrix_cv2, 
                                 cross_validation_data$Class,
                                 trainSize=NULL,
                                 testSize=1:(nrow(cross_validation_data)),
                                 virgin=TRUE)



classify_cv <- classify_models(container_cv, models)
analytics_cv <- create_analytics(container_cv, classify_cv)

predictions_raw <- analytics_cv@document_summary

# Create a simple dataframe for predictions
predictions <- data.frame(real_label=cross_validation_data$Class, 
                          predicted_label=predictions_raw$CONSENSUS_CODE,
                          algorithms_agree=predictions_raw$CONSENSUS_AGREE)

min_algs_agreeing <- 2 # We will require at least two algorightms to agree on the class to be classified


# Change our predictions dataframe, if we have at least X algorithms agreeing on the results, then 
# we will keep the prediction, if not, we will set the prediction as "N/A"
predictions$predicted_label <- ifelse(predictions$algorithms_agree >= min_algs_agreeing, predictions$predicted_label, "N/A")


# Lets print out some final conclusions
print(paste("Total number of documents to classify", nrow(cross_validation_data)))
print(paste("Total number of documents classified", nrow(subset(predictions, predicted_label!="N/A"))))
print(paste("Total number of correctly classified documents", nrow(subset(predictions, predicted_label == real_label))))
print(paste("Total number of incorrectly classified documents", nrow(subset(predictions, predicted_label != real_label))))
print(paste("Total number of documents which were not classified", nrow(subset(predictions, predicted_label == "N/A"))))


