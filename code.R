if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(fastDummies)) install.packages("fastDummies", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(corrplot)
library(readr)
library(fastDummies)
library(matrixStats)
library(gam)
library(MASS)
library(randomForest)

################################################################################
# Creating user and validation set (final hold-out test set)
################################################################################

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv","online_shoppers_intention.csv")

# ignore warning after unzip
d <- read.csv("online_shoppers_intention.csv")
class(d)

# We are going to prepare data before split

#Created Dummy variables for catagorical data and cleaning data
dat <- d %>% mutate(VisitorType = as.factor(VisitorType) ,
                            Month = as.factor(Month), 
                            Weekend = as.integer(Weekend),
                            Revenue = as.integer(Revenue)) %>% 
  dummy_columns(select_columns = c("VisitorType","Month") ) %>% 
  select(-Month , - VisitorType )



summary(dat)

# Validation set will be 10% of users data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = dat$Revenue, times = 1, p = 0.1, list = FALSE)
users <- dat[-test_index,]
validation <- dat[test_index,]

glimpse(users)



#We need to explore the different features in the dataset
mycols <- c("#0073C2FF",  "#CD534CFF")

#labels are proportion of Revenue(4 digits) in form of pie chart 
users %>% group_by(Revenue) %>%
  summarise(n=n() , prop = (n/10561)*100) %>%  #proportion of Revenue 
  ggplot(aes(x = "" ,y = prop , fill = as.factor(Revenue))) + 
  geom_bar(stat = 'identity' , width = 1 , color = "white") + 
  coord_polar("y" , start = 0)  + 
  geom_text(aes(label=sprintf("%0.2f", round(prop, digits = 2))), #Two digits after decimal in proprtion label
            position = position_stack(vjust = 0.5), 
            color = "white", size=4) +
  theme_void() + 
  scale_fill_manual(values = mycols)

#Distribution of revenue by different features through visualization

#p1 frequency of revenue by OS
p1 <- users %>%  
  ggplot(aes(as.numeric(OperatingSystems) , fill = as.factor(Revenue))) + 
  geom_histogram(bins = 15) + theme_classic() + xlab("Operating System") + 
  ggtitle("Distribution of Revenue by OS")

#p2 frequency of revenue by Browser
p2 <- users %>%  
  ggplot(aes(as.numeric(Browser) , fill = as.factor(Revenue))) + 
  geom_histogram(bins = 25) + theme_classic() + xlab("Browser") + 
  ggtitle("Distribution of Revenue by Browser") + 
  theme(legend.position = 'none')

#p3 frequency of revenue by Region
p3 <- users %>%  
  ggplot(aes(as.numeric(Region) , fill = as.factor(Revenue))) + 
  geom_histogram(bins = 17) + theme_classic() + xlab("Region") + 
  ggtitle("Distribution of Revenue by Region") + 
  theme(legend.position = 'none')

#p4 frequency of revenue by traffic type
p4 <- users %>%  
  ggplot(aes(as.numeric(TrafficType) , fill = as.factor(Revenue))) + 
  geom_histogram(bins = 40) + theme_classic() + xlab("Traffic Type") + 
  ggtitle("Distribution of Revenue by Traffic Type") + 
  theme(legend.position = 'none')

# Render the four plots in grid
p5 <- plot_grid(p1, p2, p3,p4, labels = "auto",  align="vh", ncol=2) 
title1 <- ggplot() + 
  labs(title="Figure 2. Distribution of features grouped by revenue") +
  theme(panel.background = element_blank(),
        title=element_text(size=10))
plot_grid(title1,p5, ncol=1, rel_heights=c(0.2,0.8))

################################################################################
# We will plot the distribution of some features when the revenue is true and false 
# to comapre distribution
################################################################################

#p6 Bounce rate when revenue is true
p6 <- users %>% 
  filter(Revenue == "1") %>% 
  ggplot(aes(x = BounceRates)) +
  geom_histogram() + theme_classic()

#p7 ExitRates when revenue is true
p7 <- users %>% 
  filter(Revenue == "1") %>% 
  ggplot(aes(x = ExitRates)) +
  geom_histogram() + theme_classic()

#p8 Page values when revenue is true 
p8<- users %>% 
  filter(Revenue == "1") %>% 
  ggplot(aes(x = PageValues)) +
  geom_histogram() + theme_classic()

#p9 Special day value when revenue is true
p9 <- users %>% 
  filter(Revenue == "1") %>% 
  ggplot(aes(x = SpecialDay)) +
  geom_histogram() + theme_classic()

#to then render the four plots next to each other
p10 <- plot_grid(p6, p7, p8,p9, labels = "auto",  align="vh", ncol=2)
#Adding a grid title to the four plots
title2 <- ggplot() + 
  labs(title="Figure 3. Distribution of features where Revenue is True") +
  theme(panel.background = element_blank(),
        title=element_text(size=10))
plot_grid(title2,p10, ncol=1, rel_heights=c(0.2,0.8))

######### Same features as p6,7,8,9 with revenue false #########################

p11 <- users %>% 
  filter(Revenue == "0") %>% 
  ggplot(aes(x = BounceRates)) +
  geom_histogram() + theme_classic()

p12 <- users %>% 
  filter(Revenue == "0") %>% 
  ggplot(aes(x = ExitRates)) +
  geom_histogram() + theme_classic()

p13<- users %>% 
  filter(Revenue == "0") %>% 
  ggplot(aes(x = PageValues)) +
  geom_histogram() + theme_classic()

p14 <- users %>% 
  filter(Revenue == "0") %>% 
  ggplot(aes(x = SpecialDay)) +
  geom_histogram() + theme_classic()

#to then render the four plots next to each other
p15 <- plot_grid(p11, p12, p13,p14, labels = "auto",  align="vh", ncol=2)
title3 <- ggplot() + 
  labs(title="Figure 4. Distribution of features where Revenue is False") +
  theme(panel.background = element_blank(),
        title=element_text(size=10))
plot_grid(title3,p15, ncol=1, rel_heights=c(0.2,0.8))

# Effcect of Product Related , informative and adminstration features by there respective durations

p16 <- users %>%
  ggplot(aes(Administrative , Administrative_Duration , color = as.factor(Revenue))) + 
  geom_smooth() + theme_classic()+ 
  theme(legend.position = 'none')

p17 <- users %>%
  ggplot(aes(Informational , Informational_Duration , color = as.factor(Revenue))) + 
  geom_smooth() + theme_classic()+ 
  theme(legend.position = 'none')

p18 <- users %>%
  ggplot(aes(ProductRelated , ProductRelated_Duration, color = as.factor(Revenue))) + 
  geom_smooth() + theme_classic()

#to then render the three plots next to each other
p19 <- plot_grid(p16, p17, p18, labels = "auto",  align="vh", ncol=2)
title4 <- ggplot() + 
  labs(title="Figure 5. Effect of Duration on feature by Revenue") +
  theme(panel.background = element_blank(),
        title=element_text(size=10))
plot_grid(title4,p19, ncol=1, rel_heights=c(0.2,0.8))

#correlation between features

#temp data for correlation plot
temp_dat <- data.frame(lapply(users,as.numeric))
#color pallete for correlation plot
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(temp_dat) , method="color", col=col(200),  
         type="upper", order="hclust", #hclust to order by correlation coefficiant
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         tl.cex = 0.5,
         number.cex = 0.40 , #control text size of correlation coefficiant
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#We noticed that the page related data was all highly correlated (left), 
#so we replaced those variables with the average time spent on each type of page (right) and eliminated the collinearity problem. 

AvgMinutes <- function(Count, Duration){   # function to find Average Minute for page related activity
  if (Duration == 0) {
    output = 0
  }
    
  else if ( Duration != 0) {
    output = as.numeric(Duration)/as.numeric(Count)
  }
    
  return(output) 
}


users <- users %>% # we apply the fuction for each page related feature
  rowwise() %>%
  mutate(Avg_Admin = AvgMinutes(Administrative , Administrative_Duration),
         Avg_Info = AvgMinutes(Informational,Informational_Duration),
         Avg_ProductRel = AvgMinutes(ProductRelated , ProductRelated_Duration))%>%
  ungroup() %>%
  select(-Administrative , -Administrative_Duration , -Informational , 
         -Informational_Duration , -ProductRelated , -ProductRelated_Duration)

#temp data for correlation plot after combining related features
temp_dat2 <- data.frame(lapply(users,as.numeric))
#color pallete for correlation plot
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(temp_dat2) , method="color", col=col(200),  
         type="upper", order="hclust", #hclust to order by correlation coefficiant
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         number.cex = 0.40 , #control text size of correlation coefficiant
         tl.cex = 0.5,
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#Doing the same for Validation set 
validation <- validation %>% 
  rowwise() %>%
  mutate(Avg_Admin = AvgMinutes(Administrative , Administrative_Duration),
         Avg_Info = AvgMinutes(Informational,Informational_Duration),
         Avg_ProductRel = AvgMinutes(ProductRelated , ProductRelated_Duration))%>%
  ungroup() %>%
  select(-Administrative , -Administrative_Duration , -Informational , 
         -Informational_Duration , -ProductRelated , -ProductRelated_Duration)



#Model Selection

# To select the model we will only use users set and divide it into train test set

# Validation set will be 10% of users data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = users$Revenue, times = 1, p = 0.1, list = FALSE)
train_set <- users[-test_index,]
test_set <- users[test_index,]

#convert class to factors 
train_set$Revenue <- factor(train_set$Revenue) 
test_set$Revenue <- factor(test_set$Revenue)


# Kmeans

#predict function for kmeans
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

# k means train object k  
k <- kmeans(train_set[,-10] , 2)

y_hat_kmeans <- ifelse(predict_kmeans(test_set[,-10] , k) == 1 , 1 , 0) %>% #ifelse function to create estimate class from kmeans train object
  factor()
acc_kmeans <- confusionMatrix(y_hat_kmeans , test_set$Revenue)$overall['Accuracy']

#logical Regression model
lr <- train(Revenue~. , method= 'glm' , #genralised linear model for log regression
            data = train_set)

y_hat_lr <- predict(lr , test_set[,-10])
acc_lr <- confusionMatrix(y_hat_lr , test_set$Revenue)$overall['Accuracy']

#Linear Discriminant Analysis (lda) model

lda <- train(Revenue~. , 
             method="lda" , #lga method 
             data = train_set)
y_hat_lda <- predict(lda , test_set[,-10])
acc_lda <- confusionMatrix(y_hat_lda , test_set$Revenue)$overall['Accuracy']


#Loess model

loess <- train(Revenue~. ,
               method = "gamLoess" , #Loess method 
               data = train_set)

y_hat_loess <- predict(loess , test_set[,-10])

acc_loess <- confusionMatrix(y_hat_loess , test_set$Revenue)$overall['Accuracy']

#K- Nearest Neighbour Model
tuning <- data.frame(k = seq(3, 29, 2)) #tuning parameter for value of k in knn
train_knn <- train(Revenue~.,
                   method = "knn", 
                   tuneGrid = tuning ,
                   data = train_set)

#plotting knn train object to see optimal value of k
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune

y_hat_knn <- predict(train_knn, test_set[,-10])
acc_knn <- mean(y_hat_knn == test_set$Revenue)

# Random Forest Model

# control for random forest train
control <- trainControl(method='repeatedcv', #repeated cross validation method for random forest
                        number=10,  # ten folds cross-validations
                        repeats=3, #repeated 3 times
                        search='grid') # grid search method , Each axis of grid is an algorithm parameter and point in grid are specific combinations of parameter

#this algorithm takes some time to run ; try reducing ntree or tuning parameters to reduce run time
rf <- train(Revenue~. ,
            method = 'rf' ,
            tuneGrid = data.frame(mtry = c(3,5,7,9)), #tuning parameter for mtry 
            trControl = control ,
            ntree = 501 ,  #no of trees in algorithm
            data = train_set)

# plot random forest train object to see optimal parameter
ggplot(rf, highlight = TRUE)
rf$bestTune

y_hat_rf <- predict(rf , test_set[,-10])
acc_rf <- mean(y_hat_rf == test_set$Revenue)

# table of all model accuracies
accuracy <- data_frame(Methods=c("K-Means",
                                 "Logical Regression" ,"LDA" , "Loess" ,"KNN"  , "Random Forest"),
                           Accuracy = c(acc_kmeans ,acc_lr , acc_lda,acc_loess ,acc_knn , 
                                    acc_rf))
accuracy %>% knitr::kable()

# Variable importance for random forest

ggplot(varImp(rf))

# Ensemble Model

#combine all model results (class estimates) into single matrix
models <- cbind(y_hat_kmeans , y_hat_knn , y_hat_lda , y_hat_loess , y_hat_lr , y_hat_rf)

# if majority of models predict 1(Revenue true) predict True otherwise False
y_hat_ens <- ifelse(rowMeans(models == 1) > .5 , 0 , 1 ) %>% factor()

#Accuracy of ensemble model
acc_ens <- mean(y_hat_ens == test_set$Revenue)

accuracy <- bind_rows(accuracy,
                      data_frame(Methods="Ensemble Model",  
                                 Accuracy = acc_ens))

accuracy %>% knitr::kable()                      

# Final Evaluation users v/s validation set

# As Random Forest was the most precise model we will use that for our final hold out test

users$Revenue <- factor(users$Revenue)
validation$Revenue <- factor(validation$Revenue)

# Random Forest

rf_final <- train(Revenue~. ,
            method = 'rf' ,
            tuneGrid = rf$bestTune,  #using best tune of model selection 
            trControl = control ,
            ntree = 501 ,
            data = users)

y_hat_final <- predict(rf_final , validation[,-10])
acc_final <- mean(y_hat_final == validation$Revenue)

# We can also Remove feature with low variable importance to increase the accuracy
varImp(rf_final) # importance of variables in final random forest model

# Selecting features with significant imprtance and removing low importance for users and validation set
users_selected <- users %>% 
  select(- VisitorType_Other , - Month_Feb , - Month_June , - Month_Aug , - SpecialDay , 
         -Month_Jul , - Month_Sep , - Month_Oct , - Month_Dec , -Month_Mar , -Month_May,
         - VisitorType_Returning_Visitor)
validation_selected <- validation %>% 
  select(- VisitorType_Other , - Month_Feb , - Month_June , - Month_Aug , - SpecialDay , 
         -Month_Jul , - Month_Sep , - Month_Oct , - Month_Dec , -Month_Mar , -Month_May,
         - VisitorType_Returning_Visitor)

# Training Random forest on selected features
rf_selected <- train(Revenue~. ,
                  method = 'rf' ,
                  tuneGrid = rf$bestTune,
                  trControl = control ,
                  ntree = 501 ,
                  data = users_selected)

y_hat_selected <- predict(rf_selected , validation_selected[,-9])
acc_final_selected <- mean(y_hat_selected == validation_selected$Revenue)

accuracy_final <- data_frame(Methods=c("Random Forest (users v/s validation)" ,
                                 "Random Forest Feature Selected"),
                       Accuracy = c(acc_final , acc_final_selected))
accuracy_final %>% knitr::kable()

