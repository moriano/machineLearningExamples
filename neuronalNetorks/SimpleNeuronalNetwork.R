library(neuralnet)

raw_data <- read.csv("sampleData1.csv")
head(raw_data)
train_data <- raw_data[1:75, ]
test_data <- raw_data[76:100, ]

model <- neuralnet(price ~ x + y, data=train_data, linear.output = TRUE)
plot(model)

# In order to perform predictions, we need to ensure we pass to the compute 
# function ONLY the columns that we use for the training, in this case, that 
# means the first and second columns only (they are the ones representing x and y)
predicted <- compute(model, test_data[, c(1,2)])

results <- data.frame(real=test_data$price, 
                      predictions=predicted$net.result,
                      error=test_data$price-predicted$net.result)

print(results)
print(paste("Neuronal network error is ", sum(abs(results$error))))