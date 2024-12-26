## Logistic Regression

library(dplyr)
mtcars %>% head()

str(mtcars)

## convert am to factor
mtcars$am <- factor(mtcars$am,
                    level = c(0,1),
                    labels = c("Auto", "Manual"))

class(mtcars$am)
table(mtcars$am)

## Split Data
set.seed(42)
n <- nrow(mtcars)
id <- sample(1:n, size = n*0.7)
train_data <- mtcars[id, ]
test_data <- mtcars[-id, ]

## Train Model
logit_model <- glm(am ~ mpg, data = train_data, family = "binomial")
p_train <- predict(logit_model, type = "response") # ค่าที่ออกมาจะเป็นค่า probability
train_data$pred <- if_else(p_train >= 0.5, "Manual", "Auto")
train_data$am == train_data$pred
# หา Accuracy เราใช้ mean -> True มีค่าเป็น 1 ส่วน False มีค่าเป็น 0
mean(train_data$am == train_data$pred)

## Test Model
p_test <- predict(logit_model, newdata = test_data, type = "response")
test_data$pred <- if_else(p_test >= 0.5, "Manual", "Auto")
# หา Accuracy เราใช้ mean -> True มีค่าเป็น 1 ส่วน False มีค่าเป็น 0
mean(test_data$am == test_data$pred)

## Logistic Regression Example ---------------------------------------------

happiness <- c(10, 8, 9, 7, 8, 5, 9, 6, 8, 7, 1, 1, 3, 1, 4, 5, 6, 3, 2, 0)

divorce <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

df <- data.frame(happiness, divorce)

# Fit Logistic Regression Full Dataset
model <- glm(divorce ~ happiness, data =  df, family = "binomial")

summary(model)

# Predict and Evaluate model
df$prob_divorce <- predict(model, type = "response") # type = "response" คือมันจะคำนวณออกมาเป็นความน่าจะเป็น
df$pred_divorce <- ifelse(df$prob_divorce >= 0.5, 1, 0) # เลข 1 คือ divorce เลข 0 คือไม่ divorce

# confusion matrix
conM <- table(df$pred_divorce, df$divorce,    # table เป็นฟังก์ชันที่เอาตัวแปรที่เป็น facter มา cross กันได้
      dnn = c("Predicted", "Actual")) 

# Model Evaluation
conM[1, 1] # row ที่ 1 , column ที่ 1
conM[1, 2] # row ที่ 1 , column ที่ 2
conM[2, 1] # row ที่ 2 , column ที่ 1
conM[2, 2] # row ที่ 2 , column ที่ 2

# find Accuracy
cat("Accyracy: ", (conM[1, 1] + conM[2, 2]) / sum(conM) )
# find Precision
cat("Precision: ", conM[2, 2] / (conM[2, 1] + conM[2, 2]) )
# find Recall
cat("Recall: ", conM[2, 2] / (conM[1, 2] + conM [2, 2]) )
# find F1 - Score
cat("F1 - Score: ", 2* ((0.9*0.9) / (0.9 + 0.9)) )














