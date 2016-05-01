all_data <- read.csv("sampleData1.csv")
# Quick exploration of the dataset
print(paste("Number of rows", nrow(all_data)))
head(all_data)

# First, lets split the set, 70% is for traing, 30% for testing
train_data <- all_data[0:70, ]
test_data <- all_data[71:100, ]

# Train a simple model
model <- lm(price ~ x + y, train_data)

# Predict and add two extra columns 
predictions_linear <- predict(model, test_data)

results <- data.frame(price = test_data$price, 
                      predictions_linear = predictions_linear, 
                      error_linear = test_data$price - predictions_linear)

# Lets train now a model using a polynomial level of 2
model_square <- lm(price ~ poly(x + y, 2), train_data)
predictions_square <- predict(model_square, test_data)
results$predictions_square <- predictions_square
results$error_square <- test_data$price - predictions_square

# And now lets try polynomial of level 3
model_cube <- lm(price ~ poly(x + y, 3), train_data)
predictions_cube <- predict(model_cube, test_data)
results$predictions_cube <- predictions_cube
results$error_cube <- test_data$price - predictions_cube

# Finally, lets compute the total error for each of the cases, the total error
# Is equal to the sum of the absolute values of all the errors. After that
# the lower sum will give us the better approach.

error_linear <- sum(abs(results$error_linear))
error_square <- sum(abs(results$error_square))
error_cube <- sum(abs(results$error_cube))

print(paste("Linear error is ", error_linear))
print(paste("Square error is", error_square))
print(paste("Cube error is ", error_cube))