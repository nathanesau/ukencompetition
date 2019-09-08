# Title:        Case study competition analysis code
# Authors:      Steve Kane, Nathan Esau
# Date:         Updated December 6, 2015
# Description:  Produces predictions and graphics for uken mobile game 
#               competition (Fall 2015). For predictions, the 
#               xgboost and randomForest packages were used. For
#               plots, the ggplot2 package was used.
#
# NOTE: DATA CANNOT BE UPLOADED TO GITHUB, SO THIS CODE WON'T RUN
#   LINE 21: ustats <- read.csv("Data/user_stats.csv")

library(latticeExtra)
library(randomForest)
library(rpart.plot)
library(ggplot2)
library(xgboost)
options(warn=-1)
set.seed(2015)

setwd("~/ukencompetition")
ustats <- read.csv("Data/user_stats.csv")
ustats$install_date <- as.Date(ustats$install_date, "%m/%d/%Y")

date_columns <- c("platform2_install_date", "fb_connect", "tutorial_completed",      
  "first_game_played", "first_type_1_game", "first_type_2_game", "first_type_3_game",      
  "first_type_4_game", "first_win", "first_bonus", "first_purchase_a", 
  "first_special_purchase","first_purchase_b", "first_purchase_c", "first_purchase_d", 
  "first_purchase_e", "first_purchase_f", "first_purchase_g", "first_purchase_h", 
  "first_gift_sent", "first_gift_received", "first_gift2_received", 
  "first_uken_gift_received", "first_collection", "first_prize_a", "first_prize_b", 
  "first_prize_c", "stage1", "stage2", "stage3", "stage4", "stage5", "stage6", "stage7")

stage_columns <- c("stage1", "stage2", "stage3", "stage4", "stage5", 
                   "stage6", "stage7")
ustats$StageCount <- apply(ustats[,stage_columns], 1, function(x) sum(!is.na(x)))

for(column in date_columns) {
  dates <- as.Date(ustats[,column], "%m/%d/%Y")
  ustats[,column] <- -1   # impute NA as -1 for randomForest
  
  # Number of days from install date 
  ustats[,column][!is.na(dates)] <-  as.numeric(dates[!is.na(dates)]) - 
                      as.numeric(ustats$install_date)  
}

# convert country to numeric categories for randomForest
ustats$country <- ifelse(is.na(ustats$country), -1, as.numeric(ustats$country))
ustats_train <- ustats[which(ustats$training0validation1==0),]
ustats_test <- ustats[which(ustats$training0validation1==1),]

# exploratory analysis
stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  # taken from https://gist.github.com/mrdwab/6424112
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select), function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}

graph_data <- ustats_train # create an indPay variable to stratify with
graph_data$ind_col <- ifelse(ustats_train$first_collection==-1,0,1)
graph_data$ind_pay <- ifelse(graph_data$revenue > 0,1,0)
graph_data <- stratified(graph_data, group = "ind_pay",size = c(4000))

#bubble plot
p <- ggplot(graph_data, aes(x = cut(engagement,c(0,50,200,500,1200,1800,3400),
                            include.lowest = T), y = StageCount,label = "")) +
  geom_jitter(aes(size = revenue, colour = factor(ind_col),alpha = .3),
              position = position_jitter(height = .4,width = 1)) + 
  geom_text(hjust = 2, size = 4) +
  scale_size(range = c(1,25)) +
  xlab("Engagement") +
  labs(colour = "Bonus",alpha = "Transparency",size = "Revenue")+
  scale_x_discrete("Engagement",labels = c("0-50","50-200","200-500","500-1200","1200-1800","1800-3400"))+
  theme_bw()
#show(p)

train_cols <- c("fb_connect", "tutorial_completed",
                 "first_game_played", "first_type_1_game", "first_type_2_game",
                 "first_type_3_game", "first_type_4_game", "first_win",
                 "first_bonus", "first_special_purchase", "first_purchase_a",
                 "first_purchase_b", "first_purchase_c", "first_uken_gift_received", 
                 "first_collection", "first_prize_a", "first_prize_b", 
                 "first_prize_c", "install_date", "StageCount")      

rf_revenue <- randomForest(ustats_train[,train_cols], ustats_train$revenue, ntree=100, 
                sampsize = 60000, nodesize = 20,replace = FALSE,imp=TRUE,do.trace=10)
prf_revenue <- predict(rf_revenue, ustats_test[,train_cols])
prf_revenue <- ifelse(prf_revenue > .05,prf_revenue,0) # include predictions with material dollar predicted

# revenue > 0 prediction distribution
cat("\nRevenue\n-------\n  Proportion > 0\n\tTraining set:", 
    sum(ustats_train$revenue > 0) / 250000,
    "\n\tTest set:    ", sum(prf_revenue>0)/50000,
    "\n  Mean | revenue > 0\n\tTraining set:", 
    mean(ustats_train$revenue[ustats_train$revenue>0]),
    "\n\tTest set:    ", mean(prf_revenue[prf_revenue>0]), "\n")

ecdfplot(~c(ustats_train$revenue[ustats_train$revenue>0], prf_revenue[prf_revenue>0]),
         groups=c(rep("train",length(ustats_train$revenue[ustats_train$revenue>0])),
           rep("test", length(prf_revenue[prf_revenue>0]))),  scales = list(x = list(log = 2)),
         xlab="Revenue", auto.key=list(columns=2))

# 5 most important features 
revenue_importance <- importance(rf_revenue, type=1)
revenue_importance <- revenue_importance[sort.int(revenue_importance,decreasing=T,
                                                  index.return=T)$ix,][1:5]
revenue_importance <- data.frame(Feature=names(revenue_importance), 
                                Importance=revenue_importance)

p <- ggplot(revenue_importance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  ggtitle("Revenue -- feature importance\n") +
  xlab("") + ylab("") +
  theme(plot.title=element_text(size=20))
#show(p)

# Regression tree plot
tree_columns <- c("country", "fb_connect", "tutorial_completed",
                  "first_game_played", "first_type_1_game", "first_type_2_game",      
                  "first_type_3_game", "first_type_4_game", "first_win",               
                  "first_bonus", "first_special_purchase", "first_uken_gift_received",
                  "first_collection", "first_prize_a", "first_prize_b",
                  "first_prize_c", "install_date", "BuyB", "BuyC", "StgC")       

tree_data <- ustats_train
tree_data$BuyB <- factor(ifelse(tree_data$first_purchase_b > -1, 1, 0))
tree_data$BuyC <- factor(ifelse(tree_data$first_purchase_c > -1, 1, 0))
tree_data$StgC <- factor(tree_data$StageCount)
tree <- rpart(tree_data$revenue~., data=tree_data[,tree_columns])
rpart.plot(tree)

rf_engagement <- randomForest(ustats_train[,train_cols], ustats_train$engagement, 
                  ntree=70, sampsize = 60000, nodesize = 15,replace = FALSE,imp=TRUE,do.trace=10)
prf_engagement <- predict(rf_engagement, ustats_test[,train_cols])

# engagement prediction distribution
cat("\nEngagement\n-------", 
    "\n  Mean\n\tTraining set:", 
    mean(ustats_train$engagement),
    "\n\tTest set:    ", mean(prf_engagement), "\n")

ecdfplot(~c(ustats_train$engagement, prf_engagement),
         groups=c(rep("train",length(ustats_train$engagement)),
                  rep("test", length(prf_engagement))), scales = list(x = list(log = 10)),
         xlab="Engagement", auto.key=list(columns=2))

# 5 most important features 
engagement_importance <- importance(rf_engagement, type=1)
engagement_importance <- engagement_importance[sort.int(engagement_importance,
                                    decreasing=T, index.return=T)$ix,][1:5]
engagement_importance <- data.frame(Feature=names(engagement_importance), 
                                 Importance=engagement_importance)

p <- ggplot(engagement_importance, aes(x=reorder(Feature, Importance), 
                                       y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  ggtitle("Engagement -- feature importance\n") +
  xlab("") + ylab("") +
  theme(plot.title=element_text(size=20))
#show(p)
# use ggsave() to export images in high quality

# Gradient boosting to predict return_player
# Use 60,000/250,000 rows of training set to get around memory limitations 
#   30,000 of these 60,000 rows are for training set
#   30,000 of these 60,000 rows are for validation set (cross-validation)
gb_rows <- 60000
gb_ustats <- ustats_train[sample(nrow(ustats_train), gb_rows),]
gb_train <- gb_ustats[1:(gb_rows/2),]
gb_val <- gb_ustats[(gb_rows/2+1):gb_rows,]

dtrain <- xgb.DMatrix(data.matrix(gb_train[,train_cols]), 
                      label=gb_train$return_player)
dval <- xgb.DMatrix(data.matrix(gb_val[,train_cols]), 
                    label=gb_val$return_player)
watchlist <- list(eval = dval, train = dtrain)
param <- list(objective = "binary:logistic", min_child_weight = 15,
              eta = 0.02, max_depth = 10, subsample = 0.7,
              colsample_bytree = 0.7, eval_metric = "auc")

return_player_model <- xgb.train(params = param, data = dtrain, 
                                 nrounds = 10000, verbose = 0, 
                                 early.stop.round = 20,
                                 watchlist = watchlist, maximize = TRUE)
pgbm_return_player <- predict(return_player_model,
                              data.matrix(ustats_test[,train_cols]),
                              ntreelimit = return_player_model$bestInd)

predictions <- data.frame(ID=ustats_test$user_id, engagement=prf_engagement,
  revenue=prf_revenue, return_player=round(pgbm_return_player))

write.table(predictions, file='predictions.csv')
# cor(predictions$revenue, predictions$engagement)
