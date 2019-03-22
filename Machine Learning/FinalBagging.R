library(readr)
library(C50)
library(gmodels)
library(SDMTools)
library(caret)
library(pROC)

#providing column names to the columns of the dataset and loading the dataset in R
col.names <- c('freq_make', 'freq_address', 'freq_all', 'freq_3d', 'freq_our', 'freq_over', 'freq_remove', 'freq_internet', 'freq_order',
               'freq_mail', 'freq_receive', 'freq_will', 'freq_people', 'freq_report', 'freq_addresses', 'freq_free', 'freq_business', 'freq_email',
               'freq_you', 'freq_credit', 'freq_your', 'freq_font', 'freq_000', 'freq_money', 'freq_hp', 'freq_hpl', 'freq_george', 'freq_650', 'freq_lab',
               'freq_labs', 'freq_telnet', 'freq_857', 'freq_data', 'freq_415', 'freq_85', 'freq_technology', 'freq_1999', 'freq_parts', 'freq_pm', 'freq_direct',
               'freq_cs', 'freq_meeting', 'freq_original', 'freq_project', 'freq_re', 'freq_edu', 'freq_table', 'freq_conference', 'freq_;', 'freq_(', 'freq_[',
               'freq_!', 'freq_$', 'freq_#', 'capital_run_length_average', 'capital_run_length_longest', 'capital_run_length_total', 'is_Spam')

spambase <- read.csv("spambasedata.txt", col.names=col.names, header=FALSE, stringsAsFactors = FALSE, sep=",", quote="")
spambase$is_Spam <- as.factor(spambase$is_Spam)

################################################################################################

bagging <- function (t, dataset, clf.model.type) {
  dataset <- dataset[sample(nrow(dataset)),]  
  train.data <- dataset[1:round(0.6 * nrow(dataset)), ]  
  test.data  <- dataset[round(0.6 * nrow(dataset))+1:nrow(dataset), ]  
  training.size <- nrow(train.data)
  num.cols <- ncol(train.data)
  
  aggr.results <- integer(nrow(test.data))
  for (i in 1:t) {
    random.sample <- train.data[sample(1:training.size, size = training.size,
                                       replace = TRUE), ]
    if (clf.model.type == 'decision_tree') {
      clf.model <- C5.0(random.sample[ , 1:num.cols - 1], random.sample[ , num.cols])
      test.predictions <- predict(clf.model, test.data[ , 1:num.cols - 1], type="class")
    } else {
      formula.init <- paste(colnames(dataset)[num.cols], " ~ .")
      formula.init <- as.formula(formula.init)
      clf.model <- glm(formula = formula.init, data = random.sample,
                       family = binomial(link="logit"))
      test.predictions <- predict(clf.model, test.data[ , 1:num.cols - 1], type="response")
      test.predictions <- round(test.predictions)
    }
    aggr.results <- aggr.results + as.numeric(as.character(test.predictions))
  }
  final.predicted.results <- ifelse(aggr.results > t/2, 1, 0)
  output <- list()
  output[[1]] <- final.predicted.results
  output[[2]] <- test.data[, num.cols]
  return (output)
}

main <- function (t, dataset, clf.model.type) {
  bagging.output <- bagging(t, dataset, clf.model.type)
  predictions <- bagging.output[[1]]
  actuals <- bagging.output[[2]]
  CrossTable(actuals, predictions, 
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c('actual', 'predicted'))
  cfm <- table(predictions, actuals)
  sensitivity.model <- round(sensitivity(cfm), 3)
  print(paste("Sensitivity is:", sensitivity.model), quote = FALSE)
  precision.model <- round(precision(cfm), 3)
  print(paste("Precision is:", precision.model), quote = FALSE)
  F1score.model <- round(2*((sensitivity.model*precision.model)/ (sensitivity.model+precision.model)), 4)
  print(paste("F1 score is:", F1score.model), quote = FALSE)
  roccurve <- roc(actuals, predictions)
  plot(roccurve)
  auc(roccurve)
}

