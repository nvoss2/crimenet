#load dependencies

library(lightgbm)
library(Matrix)
library(caret)
library(viridis)
library(ggmap)
library(dplyr)
#library(randomcoloR)

#load data frame
load("dfgeo.RData")
#randomize data
#seed for reproducibility
set.seed(1477)
dfgeo2 <- dfgeo %>% sample_frac()
#look at top entries
dfgeo2 %>% head()
#add month column
dfgeo2 <-select(dfgeo2,CRIME,DayOfWeek,Date,OFFENSE.TIME,Hour,COMMITTED,X,Y)
dfgeo2$month<-format(dfgeo2$Date,"%m")
#add year column
dfgeo2$year<-format(dfgeo2$Date,"%Y")
day_order<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
dfgeo2 <- dfgeo2 %>% mutate(DayOfWeek=factor(DayOfWeek,levels=day_order))
#map crime category to index
dfgeo2 <- dfgeo2 %>% mutate(category_index=as.numeric(factor(CRIME))-1)

#start using LightGBM
# declare categorical feature names
categoricals <- NULL
# proportion of data to train on
split <- 0.7
set.seed(2325)

trainIndex <- createDataPartition(dfgeo2$category_index, p = split, list = FALSE, times = 1)

dtrain <- lgb.Dataset((dfgeo2 %>% select(X, Y, Hour, month, year, DayOfWeek) %>% data.matrix())[trainIndex,],
                      colnames = c("X", "Y", "hour", "month", "year", "DayOfWeek"),
                      categorical_feature = categoricals,
                      label = dfgeo2$category_index[trainIndex], free_raw_data=T)
dtest <- lgb.Dataset.create.valid(dtrain,
                                  (dfgeo2 %>% select(X, Y, Hour, month, year, DayOfWeek) %>% data.matrix())[-trainIndex,],
                                  label = dfgeo2$category_index[-trainIndex])
params <- list(objective = "multiclass", metric = "multi_logloss")
valids <- list(test=dtest)
num_classes <- length(unique(dfgeo2$category_index))
# preformat sizes for use in data visualizations later
train_size_format <- length(trainIndex) %>% format(big.mark=",")
test_size_format <- (dfgeo2 %>% nrow() - length(trainIndex)) %>% format(big.mark=",")


# determine elapsed runtime 
system.time(
  # training output not printed to notebook since spammy. (verbose = 0 + record = T)
  bst <- lgb.train(params,
                   dtrain,
                   nrounds = 500,
                   valids,
                   num_threads = 4,
                   num_class = num_classes,
                   verbose = 0,
                   record = T,
                   early_stopping_rounds = 5,
                   categorical_feature = categoricals
  )
)[3]

#elapsed: 41.947

# multilogloss of final iteration on test set
paste("# Rounds:", bst$current_iter())
# Rounds: 500

paste("Multilogloss of best model:", bst$record_evals$test$multi_logloss$eval %>% unlist() %>% tail(1))
#Multilogloss of best model: 1.90277012439858

#Calculate variable importance
df_imp <- tbl_df(lgb.importance(bst, percentage = TRUE))

#preds is a 1D vector of probabilities for each vector, of nrows x nclasses. Reshape accordingly and iterate through for the predicted label (label with the largest probability) and the corresponding probability.
test <- (dfgeo2 %>% select(X, Y, Hour, month, year, DayOfWeek) %>% data.matrix())[-trainIndex,]
preds_matrix <- predict(bst, test, reshape=T)
preds_cor <- cor(preds_matrix)
preds_matrix[1:2,]

#get the prediction (label with highest predicted probability)
results <- t(apply(preds_matrix, 1, function (x) {
  max_index = which(x==max(x))
  return (c(max_index-1, x[max_index]))
}))

#compare predicted crime categories to actual
df_results <- data.frame(results, label_act = dfgeo2$category_index[-trainIndex]) %>%
  tbl_df() %>%
  transmute(label_pred = X1, prod_pred = X2, label_act)
df_results %>% arrange(desc(prod_pred)) %>% head(20)

cm <- confusionMatrix(df_results$label_pred, df_results$label_act)
data.frame(cm$overall)

#visualizations
df_imp$Feature <- factor(df_imp$Feature, levels=rev(df_imp$Feature))

mlplot <- ggplot(df_imp, aes(x=Feature, y=Gain)) +
  geom_bar(stat="identity", fill="#34495e", alpha=0.9) +
  geom_text(aes(label=sprintf("%0.1f%%", Gain*100)), color="#34495e", hjust=-0.25, family="Open Sans Condensed Bold", size=2.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 0.4), labels=c("0%","10%","20%","30%","40%")) +
  theme(plot.title=element_text(hjust=0.5), axis.title.y=element_blank()) +
  labs(title="Feature Importance for Baton Rouge Crime Type Model", y="% of Total Gain in LightGBM Model")




#plot the confusion matrix
df_cm <- tbl_df(data.frame(cm$table))
df_cm %>% head(100)



# create mapping df
df_labels <- dfgeo2 %>%
  select(category_index, CRIME) %>%
  group_by(category_index, CRIME) %>%
  summarize() %>%
  ungroup() %>%
  mutate(category_index = factor(category_index))

df_cm <- df_cm %>%
  left_join(df_labels, by = c("Prediction" = "category_index")) %>%
  left_join(df_labels, by = c("Reference" = "category_index")) %>%
  rename(label_pred = CRIME.x, label_act = CRIME.y)
df_cm %>% head(100)