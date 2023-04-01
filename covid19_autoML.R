# The libraries
library(caret)
library(h2o)

# AutoML (Setting up connection cluster)
h2o.init()

# Splitting the dataset into train and test sets
set.seed(1)
sampleSplit<- createDataPartition(df$PCR_outcome, p = .75, list = F)
train<- as.h2o(df[sampleSplit,])
test<- as.h2o(df[-sampleSplit,])




y <- "PCR_outcome"
x <- setdiff(names(train), y)

# Run AutoML for 20 base models
aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_models = 20,
                  seed = 1)

# View the AutoML Leader board
lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

# Get leader board with all possible columns
ldb <- h2o.get_leaderboard(object = aml)

# Obtain every information about the best model
aml@leader

# Get the best gbm model using default sort metric
gbm <- h2o.get_best_model(aml, algorithm = "gbm");gbm

# Obtain the hyper-parameters
gbm@params$actual

# Variable importance (Not available for Stacked Ensembles)
h2o.varimp(gbm)
h2o.varimp_plot(gbm) # The variable importance plot for GBM model
h2o.varimp_heatmap(aml) # Variable importance heat-map for each model
h2o.shap_summary_plot(gbm, test) # Shap summary plot for feature importance

# Shap explain plot for feature importance
h2o.shap_explain_row_plot(gbm, test, row_index = 1) 


# Making predictions using the best model (Stacked Ensemble model)
pred <- h2o.predict(aml@leader, test)
testPred<- as.data.frame(as.factor(pred$predict))
original<- as.data.frame(as.factor(test$PCR_outcome))

# Evaluating the prediction using Caret pkg
confusionMatrix(testPred$predict,original$PCR_outcome)

