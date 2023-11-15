pacman::p_load(lubridate,dplyr,pdp,vip,xts,ggplot2,caTools,randomForest,dygraph,gridExtra,RColorBrewer,Metrics) 
library(ggplot2)
library(randomForest)
library(pdp,lattice)
library(caTools)
setwd("C:/Users/THAPELO T SWAABOW/Desktop/Projecto/Codigos");

# Link to the local csv files by sourcing the R files for that (Local and Google Drive)
################################################################################################
source("C:/Users/THAPELO T SWAABOW/Desktop/Projecto/Codigos/DatasetGeneratorRandomForest.R")
Datozz<-DataSet_Genator();Datozz[is.na(Datozz)] = 0 # Fill missing values with zeros

View(Datozz)

################################################################################################
# Pull Mobility data and stringency indices
TimeSerz <- Datozz[Datozz$date >= "2020-02-17" & Datozz$date <= "2022-08-10", ] %>% 
  select(c('date',"Residential", "Workplaces", "Retail_and_Recreation", "Grocery_and_Pharmacy",
           "Transit_Stations", "Parks",'Stringency_Index',"Containment_Index","New_Cases","New_Deaths"))

#********************************* Plotting Mobility Data Patterns ************************************************************
#*
don=xts( x=TimeSerz[,-c(1,4,5,6,7,10,11)], order.by=TimeSerz$date)

# Checking the effect of the aggrated stringencies on Residential and Workplaces indicators
p1 <- dygraph(don,   ylab = "Index values in % (30-day moving average)", xlab = "Time")%>%
  dyLegend(width = 600)%>%
  dyRoller(rollPeriod = 30)%>% 
  #  pointShape = c("dot", "triangle",) %>% # "square", "diamond", "pentagon", "hexagon", "circle", "star", "plus", "ex"),
  dySeries("Residential", label = "Residential", color = "blue", pointShape = c("triangle")) %>%
  dySeries("Workplaces", label = "Workplaces", color = "green", pointShape = c("triangle")) %>%
  dySeries("Residential", label = "Residential", color = "green", pointShape = c("triangle")) %>%
  dySeries("Stringency_Index", label = "SI", color = "orange") %>%
  dySeries("Containment_Index", label = "CHI ", color = "red") #%>%
# dyShading(from = "2020-08-21", to = "2020-09-12") %>%
#  dyAnnotation("2020-08-21", text = "M", tooltip = "Missing start here") %>%
#  dyEvent("2020-08-21", "Missing values", labelLoc = "top")
#Label axies, change the colours, define the legend
#dyOptions( stepPlot=TRUE, fillGraph=FALSE)%>%
# dyCandlestick()

p1


don2=xts( x=TimeSerz[,-c(1,2,3,10,11)], order.by=TimeSerz$date)

p2 <- dygraph(don2,   ylab = "Index values in % (30-day moving average)", xlab = "Time")%>%
  dyLegend(width = 800)%>%
  dyRoller(rollPeriod = 30)%>% 
  #  pointShape = c("dot", "triangle",) %>% # "square", "diamond", "pentagon", "hexagon", "circle", "star", "plus", "ex"),
  dySeries("Retail_and_Recreation", label = "Retail_and_Recreation", color = "blue", pointShape = c("triangle")) %>%
  dySeries("Grocery_and_Pharmacy", label = "Grocery_and_Pharmacy", color = "green", pointShape = c("triangle")) %>%
  dySeries("Transit_Stations", label = "Transit_Stations", color = "green", pointShape = c("triangle")) %>%
  dySeries("Parks", label = "Parks", color = "black") %>%
  dySeries("Stringency_Index", label = "SI", color = "orange") %>%
  dySeries("Containment_Index", label = "CHI ", color = "red") #%>%
# dyShading(from = "2020-08-21", to = "2020-09-12") %>%
#  dyAnnotation("2020-08-21", text = "M", tooltip = "Missing start here") %>%
#  dyEvent("2020-08-21", "Missing values", labelLoc = "top")
#Label axies, change the colours, define the legend
#dyOptions( stepPlot=TRUE, fillGraph=FALSE)%>%
# dyCandlestick()

p2


don3=xts( x=TimeSerz[,-c(1,3,4,5,6,11)], order.by=TimeSerz$date)



p3 <- dygraph(don3,   ylab = "Index values in % (7-day moving average)", xlab = "Time")%>%
  dyLegend(width = 600)%>%
  dyRoller(rollPeriod = 30)%>% 
  #  pointShape = c("dot", "triangle", "square", "diamond", "pentagon", "hexagon", "circle", "star", "plus", "ex"),
  
  dyAxis("y", label = "30-day moving average of New COVID-19 Cases in blue", independentTicks = TRUE) %>%
  dyAxis("y2", label = "30-day moving average of Parks, Residential, and CHI", independentTicks = TRUE) %>%
  dySeries("New_Cases", axis=('y'), label = "New COVID-19 Cases (NCCs)", color = "blue") %>%
  dySeries("Residential", axis=('y2'), label = "Residential", color = "green") %>%
  dySeries("Parks", axis=('y2'), label = "Parks", color = "yellow") %>%
  
  #dySeries("new_deaths", axis=('y'), color = "yellow") %>%
  dyEvent("2020-03-16", "Mar, 16: Official Public Notice on COVID-19    ", labelLoc = "top") %>%
   
  dyEvent("2020-03-31", "Mar, 31: State of Emergency    ", labelLoc = "top") %>%
  
  dyEvent("2020-05-08", "Lift Transport Restrictions        ", labelLoc = "top") %>%
 # dyEvent("2020-05-11", "[1. Lockdown of Gaborone];    [Activate Transport Restrictions]       ", labelLoc = "top") %>%
 # dyEvent("2020-06-02",  "Jun, 02: Lift  Liquor Restrictions       ", labelLoc = "bottom") %>%
  dyEvent("2020-06-05",  "Jun, 05: Relax Business and Trading Hours       ", labelLoc = "top") %>%
  
 # dyEvent("2020-06-12", "Lockdown (Greater Gaborone Zone)       ", labelLoc = "top") %>%
  dyEvent("2020-08-05", "Aug, 5: Ban Sale of Liquor       ", labelLoc = "bottom") %>%
  dyEvent("2020-08-13", "Aug, 13: Lift Gathering Restrictions      ", labelLoc = "top") %>%
  
  
  
  dyEvent("2020-09-04", "Sep, 04: Lift Liquor Restrictions", labelLoc = "top") %>%
  dyEvent("2020-09-18", "Sep, 18: Activate Entertainment", labelLoc = "bottom") %>%

  dyEvent("2021-01-03", "Activate Liquor Restrictions      ", labelLoc = "top") %>%
  dyEvent("2021-01-03", "Jan, 03  Activate  Movement Restrictions     ", labelLoc = "bottom") %>%
 # dyEvent("2021-02-26", "Activate  Liquor Restrictions       ", labelLoc = "top") %>%
  dyEvent("2021-02-26", "Feb, 26: Activate  Liquor Restrictions", labelLoc = "bottom") %>%
  
  #dyEvent("2022-02-28", "Lift Movement Restrictions      ", labelLoc = "top") %>%
  #dyEvent("2022-02-28", "Feb, 28", labelLoc = "bottom") %>%
  #dyEvent("2022-03-01", "Lift Liquor Restrictions      ", labelLoc = "top") %>%
  
  dySeries("Stringency_Index", axis=('y2'), stepPlot = F,label = "SI", color = "orange")%>%
  dySeries("Containment_Index", axis=('y2'), stepPlot = F,label = "CHI", color = "red")
# dyShading(from = "2020-08-21", to = "2020-09-12") %>%
#  dyAnnotation("2020-08-21", text = "M", tooltip = "Missing start here") %>%
#  dyEvent("2020-08-21", "Missing values", labelLoc = "top")
#Label axies, change the colours, define the legend
#dyOptions( stepPlot=TRUE, fillGraph=FALSE)%>%
# dyCandlestick()
#dyOptions( stemPlot=TRUE)
p3

#select(c('date',"Residential", "Workplaces", "Retail_and_Recreation", "Grocery_and_Pharmacy",
#"Transit_Stations", "Parks",'Stringency_Index',"Containment_Index","New_Cases","New_Deaths"))


don4=xts( x=TimeSerz[,-c(1,2,7,8,10)], order.by=TimeSerz$date)

p4 <- dygraph(don4,   ylab = "Index values in % (7-day moving average)", xlab = "Time")%>%
  dyLegend(width = 800)%>%
  dyRoller(rollPeriod = 30)%>% 
  #  pointShape = c("dot", "triangle", "square", "diamond", "pentagon", "hexagon", "circle", "star", "plus", "ex"),
  
  dyAxis("y", label = "30-day moving average of New COVID-19 Deaths in green", independentTicks = TRUE) %>%
  dyAxis("y2", label = "30-day moving average of Mobility Indices, CHI and SI", independentTicks = TRUE) %>%
  dySeries("New_Deaths", axis=('y'), label = "New COVID-19 Deaths", color = "green") %>% 
  dySeries("Workplaces", axis=('y2'), label = "Workplaces", color = "yellow") %>%
  dySeries("Retail_and_Recreation", axis=('y2'), label = "Retail_and_Recreation", color = "blue") %>%
  dySeries("Grocery_and_Pharmacy", axis=('y2'), label = "Grocery_and_Pharmacy", color = "brown") %>%
  dySeries("Transit_Stations", axis=('y2'), label = "Transit_Stations", color = "black") %>%
  
  #dySeries("new_deaths", axis=('y'), color = "yellow") %>%
  dyEvent("2020-03-16", "Mar, 16: Official Public Notice on COVID-19    ", labelLoc = "top") %>%
  
  dyEvent("2020-03-31", "Mar, 31: State of Emergency    ", labelLoc = "top") %>%
  
  dyEvent("2020-05-08", "Lift Transport Restrictions        ", labelLoc = "top") %>%
  # dyEvent("2020-05-11", "[1. Lockdown of Gaborone];    [Activate Transport Restrictions]       ", labelLoc = "top") %>%
  # dyEvent("2020-06-02",  "Jun, 02: Lift  Liquor Restrictions       ", labelLoc = "bottom") %>%
  dyEvent("2020-06-05",  "Jun, 05: Relax Business and Trading Hours       ", labelLoc = "top") %>%
  
  # dyEvent("2020-06-12", "Lockdown (Greater Gaborone Zone)       ", labelLoc = "top") %>%
  dyEvent("2020-08-05", "Aug, 5: Ban Sale of Liquor       ", labelLoc = "bottom") %>%
  dyEvent("2020-08-13", "Aug, 13: Lift Gathering Restrictions      ", labelLoc = "top") %>%
  
  
  
  dyEvent("2020-09-04", "Sep, 04: Lift Liquor Restrictions", labelLoc = "top") %>%
  dyEvent("2020-09-18", "Sep, 18: Activate Entertainment", labelLoc = "bottom") %>%
  
  dyEvent("2021-01-03", "Activate Liquor Restrictions      ", labelLoc = "top") %>%
  dyEvent("2021-01-03", "Jan, 03  Activate  Movement Restrictions     ", labelLoc = "bottom") %>%
  # dyEvent("2021-02-26", "Activate  Liquor Restrictions       ", labelLoc = "top") %>%
  dyEvent("2021-02-26", "Feb, 26: Activate  Liquor Restrictions", labelLoc = "bottom") %>%
  
  #dyEvent("2022-02-28", "Lift Movement Restrictions      ", labelLoc = "top") %>%
  #dyEvent("2022-02-28", "Feb, 28", labelLoc = "bottom") %>%
  #dyEvent("2022-03-01", "Lift Liquor Restrictions      ", labelLoc = "top") %>%
  
  #dySeries("Stringency_Index", axis=('y2'), stepPlot = F,label = "SI", color = "orange")%>%
  dySeries("Containment_Index", axis=('y2'), stepPlot = F,label = "CHI", color = "red")
# dyShading(from = "2020-08-21", to = "2020-09-12") %>%
#  dyAnnotation("2020-08-21", text = "M", tooltip = "Missing start here") %>%
#  dyEvent("2020-08-21", "Missing values", labelLoc = "top")
#Label axies, change the colours, define the legend
#dyOptions( stepPlot=TRUE, fillGraph=FALSE)%>%
# dyCandlestick()
#dyOptions( stemPlot=TRUE)
p4



#library(manipulateWidget)
#combineWidgets(p3, p3, ncol = 2)

################################################################################################
New.Death.df <- Datozz[Datozz$date >= "2020-02-17" & Datozz$date <= "2021-12-31", ] %>% 
  select(-c('date','Stringency_Index',"Containment_Index"))
New.Cases.df <- Datozz[Datozz$date >= "2020-02-17" & Datozz$date <= "2021-12-31", ] %>% 
  select(-c('date','New_Deaths','Stringency_Index',"Containment_Index"))
dim(New.Cases.df)
################################################################################################
# Percentage spliting
train_test_split <- function(df){
  set.seed(23)
  sample = sample.split(df, SplitRatio = 0.8)
  train = subset(df, sample == TRUE)
  test  = subset(df, sample == FALSE)
  return (list(train, test))
}
set.seed(23)
# New COVID-19 Positive Cases
New.Case.df_train <- train_test_split(New.Cases.df)[[1]]
New.Case.df_test <- train_test_split(New.Cases.df)[[2]]
dim(New.Case.df_test)
dim(New.Case.df_test)

# New COVID-19 Deaths
New.Death.df_train <- train_test_split(New.Death.df)[[1]]
New.Death.df_test <- train_test_split(New.Death.df)[[2]]
dim(New.Death.df_train)
dim(New.Death.df_test)

################################################################################
# Tune the parameters for a random forest

#fgl.res <- tuneRF(New.Death.df_train[,-1], New.Death.df_train[,1], stepFactor=1.5)

################################################################################################
# Fit a random forest to the mtcars dataset
set.seed(23)

#Tune the Random Forest  Model


# Tune RF model for New COVID-19 deaths
#set.seed(23)
#model_tuned_NCCs <- tuneRF(
#  x=New.Case.df_train[,-1], #define predictor variables
#  y=New.Case.df_train$New_Cases, #define response variable
#  ntreeTry=500,
#  mtryStart=15, 
#  stepFactor=1.5,
#  improve=0.01,
#  trace=FALSE #don't show real-time progress
#)

# Tune RF model for New COVID-19 deaths
#set.seed(23)
#model_tuned_NCDs <- tuneRF(
#  x=New.Death.df_train[,-1], #define predictor variables
#  y=New.Death.df_train$New_Deaths, #define response variable
#  ntreeTry=500,
#  mtryStart=15, 
#  stepFactor=1.5,
#  improve=0.01,
#  trace=FALSE #don't show real-time progress
#)

#-----------------------------------
set.seed(23)
NCC.rf <- randomForest(New_Cases~ ., data = New.Case.df_train, nodeSize  =  5, mtry = 1,importance = TRUE); NCC.rf
NCD.rf <- randomForest(New_Deaths~ ., data = New.Death.df_train, nodeSize  =  5, mtry = 457, importance = TRUE); NCD.rf
 
NCC_Trees <- plot(NCC.rf); NCC_Trees
NCD_Trees <- plot(NCD.rf); NCD_Trees



#Return the Training MSE
NCC.rf$mse[which.min(NCC.rf$mse)]
NCD.rf$mse[which.min(NCD.rf$mse)]


#Find number of trees that produce lowest test MSE
which.min(NCC.rf$mse)
which.min(NCD.rf$mse)

 

#find RMSE of best model
sqrt(NCC.rf$mse[which.min(NCC.rf$mse)]) 
sqrt(NCD.rf$mse[which.min(NCD.rf$mse)])

# Computing the errors on a test data sets
# Predict using the test set
prediction_Test_NCCs_A <- predict(NCC.rf, New.Case.df_test)
prediction_Test_NCDs_B <- predict(NCD.rf, New.Death.df_test)


MSEras_NCCs <- sqrt(mean((prediction_Test_NCCs_A)))
MSEras_NCDs <- sqrt(mean((prediction_Test_NCDs_B)))
mean((prediction_Test_NCCs_A)); mean((prediction_Test_NCDs_B))
MSEras_NCCs;MSEras_NCDs
 


# Save the solution to a dataframe with two columns: NCDs Actual and NCDs (prediction)
solutionA <- data.frame(New_Cases = New.Case.df_test$New_Cases, Predicted_NCCs = prediction_Test_NCCs_A)
solutionB <- data.frame(New_Deaths = New.Death.df_test$New_Deaths, Predicted_NCDs = prediction_Test_NCDs_B)

data1 <- data.frame(
  indexo = as.numeric(seq.int(from=0,to=499,by=1)),
  solutionA2 <-  data.frame(as.numeric(log(NCC.rf$mse)))
  
)

data2 <- data.frame(
  indexo = as.numeric(seq.int(from=0,to=499,by=1)),
  solutionB2 <-  data.frame(as.numeric(log(NCD.rf$mse)))
  
)

 
NCCs_Grafxx <- ggplot(data1, aes(x =  indexo, y = data1$as.numeric.log.NCC.rf.mse..)) +
  #geom_point() +
  geom_line(size = 1) +
 # stat_smooth(#method = "loess",
  #            col = "#C42126",
  #            se = TRUE,
  #            size = 1) +
  labs(
    #title = paste("Training Error=", mean(NCC.rf$mse), "Mtry=", NCC.rf$mtry),
    x = "Trees",
    y = "Error on Testing Batch 3 (NCCs)"
  ) +
  theme_bw() 
NCCs_Grafxx

NCDs_Grafxx <- ggplot(data2, aes(x =  indexo, y = data2$as.numeric.log.NCD.rf.mse..)) +
  #geom_point() +
  geom_line(size = 1) +
  # stat_smooth(#method = "loess",
  #            col = "#C42126",
  #            se = TRUE,
  #            size = 1) +
  labs(
   # title = paste("Actual and Predicted NCCs"),
    x = "Trees",
    y = "Error on Testing Batch 3 (NCDs)"
  ) +
  theme_bw() 
NCDs_Grafxx

#*******************************************************************************
# Plot the scatter plot of prediction vs actual for New COVID-19 deaths
 
mean_NCCs <- mean(New.Case.df_test$New_Cases)
NCCs_Graf1 <- ggplot(solutionA, aes(x = log(New_Cases), y = log(Predicted_NCCs))) +
  geom_point() +
  stat_smooth(method = "loess",
              col = "#C42126",
              se = TRUE,
              size = 1) +
  labs(
   # title = paste("Actual and Predicted"),
    x = "Log of Actual NCCs",
    y = "Log of Predicted  NCCs (Validation)"
  ) +
  theme_bw() #+
NCCs_Graf1 

NCCs_Graf1B <- ggplot(solutionA, aes(x =  (New_Cases), y =  (Predicted_NCCs))) +
  geom_point() +
   geom_smooth(method = "lm", se = TRUE) +
  labs(
   # title = paste("Actual and Predicted"),
    x = "Actual  NCCs",
    y = "Predicted  NCCs on Validation"
  ) +
  theme_bw() #+
NCCs_Graf1B 


# Plot the scatter plot of prediction vs actual for New COVID-19 deaths
mean_New_Deaths <- mean(New.Death.df_test$New_Deaths)
NCDs_Graf2 <- ggplot(solutionB, aes(x =  log(New_Deaths), y =  log(Predicted_NCDs))) +
  geom_point() +
  stat_smooth(method = "loess",
              col = "#C42126",
              se = TRUE,
              size = 1) +
  labs(
   # title = paste("Actual and Predicted"),
    x = "Log of Actual NCDs",
    y = "Log of Predicted NCDs (Validation)"
  )  +
  theme_bw()
NCDs_Graf2  

 
NCDs_Graf3 <- ggplot(solutionB, aes(x =   (New_Deaths), y =   (Predicted_NCDs))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_smooth(method = "loess",
              col = "#C42126",
              se = TRUE,
              size = 1) +
  labs(
  #  title = paste("Actual and Predicted"),
    x = "Actual NCDs",
    y = "Predicted NCDs on Validation"
  )  +
  theme_bw()
NCDs_Graf3   
 

grid.arrange(NCCs_Grafxx, NCCs_Graf1B, NCCs_Graf1, NCDs_Grafxx,  NCDs_Graf3, NCDs_Graf2, ncol = 3)
#*******************************************************************************


# Make predictions using the training datasets
pred_test_NCCs <- predict(NCC.rf, newdata = New.Case.df_test); pred_test_NCCs
pred_Training_NCDs <- predict(NCD.rf, newdata = New.Death.df_train)
plot(New.Case.df_test$New_Cases, predict(NCC.rf, newdata = New.Case.df_test[,-1]))
abline (0, 1) 



# Make predictions
pred_test_NCCs <- predict(NCC.rf, newdata = New.Case.df_test)
pred_test_NCDs <- predict(NCD.rf, newdata = New.Death.df_test)

plot(New.Case.df_test$New_Cases, predict(NCC.rf, newdata = New.Case.df_test[,-1]))
abline (0, 1) 

plot(NCC.rf$predicted)
NCCs_MSE <- plot(NCC.rf$mse, type = "l", xlab = "Trees",ylab = "Error (MSE)")


yhat.bag <- predict(RF.NCC , newdata = New.case.df[-NCC_train, ])
plot(yhat.bag , NCC_test)
abline (0, 1)
mean (( yhat.bag - NCC_test)^2)




################################################################################
# Plot the  variable importance measures
vi(NCC.rf)
vi(NCD.rf)

vip_NDCs <- vip(NCD.rf,aesthetics = list(color = "black", fill = "white")) + 
  ggtitle("NCDs: 20-02-17 to 22-08-10")  
plot(vip_NDCs)

vip_NCCs <- vip(NCC.rf,aesthetics = list(color = "black", fill = "white")) + 
  ggtitle("NCCs: 20-02-17 to 22-08-10");
plot(vip_NCCs)

grid.arrange(vip_NCCs, vip_NDCs, ncol = 2)
################################################################################
# Top n predictors
top6_NCDs <- topPredictors(NCD.rf, n = 6); top6_NCDs
top6_NCCs <- topPredictors(NCC.rf, n = 6); top6_NCCs

################################################################################
# Construct partial dependence functions for top four predictors
top6_PDP_NCDs <- NULL
for (i in top6_NCDs) {
  tmp <- partial(NCD.rf, pred.var = i)
  names(tmp) <- c("x", "y")
  top6_PDP_NCDs <- rbind(top6_PDP_NCDs,  cbind(tmp, predictor = i))
}
# Display partial dependence functions
top6_NCDs_pdp <- ggplot(top6_PDP_NCDs, aes(x, y)) + geom_line() + 
  facet_wrap(~ predictor, scales = "free") + geom_smooth()+ theme_bw() +
  ylab("New COVID-19 Deaths") +  xlab("Predictor Variables") 
top6_NCDs_pdp
#*******************************************************************************
top6_PDP_NCCs <- NULL
for (i in top6_NCCs) {
  tmp <- partial(NCC.rf, pred.var = i)
  names(tmp) <- c("x", "y")
  top6_PDP_NCCs <- rbind(top6_PDP_NCCs,  cbind(tmp, predictor = i))
}
# Display partial dependence functions
top6_NCCs_pdp <- ggplot(top6_PDP_NCCs, aes(x, y)) + geom_line() + 
  facet_wrap(~ predictor, scales = "free") + geom_smooth()+ theme_bw() +
  ylab("Predicted New COVID-19 Cases") +  xlab("Predictor Variables") 
top6_NCCs_pdp
################################################################################
# Plot the ICEbox for Grocery_and_Pharmacy
require(ICEbox)
#create iceplot object
iceplot1 = ice(object = NCC.rf, 
               X = New.Cases.df, 
               y = New.Cases.df$New_Cases,   
               predictor = "Grocery_and_Pharmacy")
set.seed(23)
iceplot_NCCs_GP <- plot(iceplot1, 
                        frac_to_plot = 0.1, 
                        centered = T, 
                        plot_orig_pts_preds = T, 
                        color_by = "Grocery_and_Pharmacy", lwd=2,   type="l", xlab = "Grocery_and_Pharmacy", ylab = "Predicted New COVID-19 Cases") 
iceplot_NCCs_GP
#********************************************************************************************************** 

#create iceplot object
iceplot2 = ice(object = NCC.rf, 
               X = New.Cases.df, 
               y = New.Cases.df$New_Cases,  
               predictor = "Parks")
set.seed(23)
iceplot_NCCs_Parks <-plot(iceplot2, 
                          frac_to_plot = 0.1, 
                          centered = T, 
                          plot_orig_pts_preds = T, 
                          color_by = "Parks", lwd=2,   type="l", xlab = "Parks", ylab = "Predicted New COVID-19 Cases") 
iceplot_NCCs_Parks
#********************************************************************************************************** 

#create iceplot for Positive_Rate
iceplot3 = ice(object = NCC.rf, 
               X = New.Cases.df, 
               y = New.Cases.df$New_Cases,  
               predictor = "Residential")
set.seed(23)
iceplot_NCCs_PR <- plot(iceplot3, 
                        frac_to_plot = 0.1, 
                        centered = T, 
                        plot_orig_pts_preds = T, 
                        color_by = "Residential", lwd=2,   type="l", xlab = "Residential", "Predicted New COVID-19 Cases") 
iceplot_NCCs_PR
#********************************************************************************************************** 

#********************************************************************************************************** 

#create iceplot object
iceplot6 = ice(object = NCC.rf, 
               X = New.Cases.df, 
               y = New.Cases.df$New_Cases,   
               predictor = "Transit_Stations")
set.seed(23)
iceplot_NCCs_TS <- plot(iceplot6, 
                        frac_to_plot = 0.1, 
                        centered = T, 
                        plot_orig_pts_preds = T, 
                        color_by = "Transit_Stations", lwd=2,   type="l", xlab = "Transit_Stations",ylab = "Predicted New COVID-19 Cases") 
iceplot_NCCs_TS


#create iceplot object
iceplot7 = ice(object = NCC.rf, 
               X = New.Cases.df, 
               y = New.Cases.df$New_Cases,   
               predictor = "Workplaces")
set.seed(23)
NCCs_WorkP <- plot(iceplot7, 
                        frac_to_plot = 0.1, 
                        centered = T, 
                        plot_orig_pts_preds = T, 
                        color_by = "Workplaces", lwd=2,   type="l", xlab = "Workplaces",ylab = "Predicted New COVID-19 Cases") 
NCCs_WorkP



#create iceplot object
iceplot7 = ice(object = NCC.rf, 
               X = New.Cases.df,  
               y = New.Cases.df$New_Cases,   
               predictor = "Positive_Rate")
set.seed(23)
NCCs_WorPR <- plot(iceplot7, 
                   frac_to_plot = 0.1, 
                   centered = T, 
                   plot_orig_pts_preds = T, 
                   color_by = "Positive_Rate", lwd=2,   type="l", xlab = "Positive_Rate",ylab = "Predicted New COVID-19 Cases") 
NCCs_WorPR


#create iceplot object
iceplot8 = ice(object = NCC.rf, 
               X = New.Cases.df,  
               y = New.Cases.df$New_Cases,   
               predictor = "Reproduction_Rate")
set.seed(23)
NCCs_WorRR <- plot(iceplot8, 
                   frac_to_plot = 0.1, 
                   centered = T, 
                   plot_orig_pts_preds = T, 
                   color_by = "Reproduction_Rate", lwd=2,   type="l", xlab = "Reproduction_Rate",ylab = "Predicted New COVID-19 Cases") 
NCCs_WorRR



#create iceplot object
iceplot9 = ice(object = NCC.rf, 
               X = New.Cases.df,  
               y = New.Cases.df$New_Cases,   
               predictor = "School_Closures")
set.seed(23)
NCCs_WorSC <- plot(iceplot9, 
                   frac_to_plot = 0.1, 
                   centered = T, 
                   plot_orig_pts_preds = T, 
                   color_by = "School_Closures", lwd=2,   type="l", xlab = "School_Closures",ylab = "Predicted New COVID-19 Cases") 
NCCs_WorSC



par(mfrow = c(2, 3))

# The following two plots will be combined
iceplot_NCCs_GP <- plot(iceplot1, 
                        frac_to_plot = 0.1, 
                        centered = T, 
                        plot_orig_pts_preds = T, 
                        color_by = "Grocery_and_Pharmacy", lwd=2,   type="l", xlab = "Grocery_and_Pharmacy", ylab = "Predicted New COVID-19 Cases") 
iceplot_NCCs_GP    # Left 

NCCs_WorPR <- plot(iceplot7, 
                   frac_to_plot = 0.1, 
                   centered = T, 
                   plot_orig_pts_preds = T, 
                   color_by = "Positive_Rate", lwd=2,   type="l", xlab = "Positive_Rate",ylab = "Predicted New COVID-19 Cases") 
NCCs_WorPR

NCCs_WorRR <- plot(iceplot8, 
                   frac_to_plot = 0.1, 
                   centered = T, 
                   plot_orig_pts_preds = T, 
                   color_by = "Reproduction_Rate", lwd=2,   type="l", xlab = "Reproduction_Rate",ylab = "Predicted New COVID-19 Cases") 
NCCs_WorRR




iceplot_NCCs_Parks <-plot(iceplot2, 
                          frac_to_plot = 0.1, 
                          centered = T, 
                          plot_orig_pts_preds = T, 
                          color_by = "Parks", lwd=2,   type="l", xlab = "Parks", ylab = "Predicted New COVID-19 Cases") 
iceplot_NCCs_Parks


iceplot_NCCs_TS <- plot(iceplot6, 
                        frac_to_plot = 0.1, 
                        centered = T, 
                        plot_orig_pts_preds = T, 
                        color_by = "Transit_Stations", lwd=2,   type="l", xlab = "Transit_Stations",ylab = "Predicted New COVID-19 Cases") 
iceplot_NCCs_TS # Right



NCCs_WorkP <- plot(iceplot7, 
                   frac_to_plot = 0.1, 
                   centered = T, 
                   plot_orig_pts_preds = T, 
                   color_by = "Workplaces", lwd=2,   type="l", xlab = "Workplaces",ylab = "Predicted New COVID-19 Cases") 
NCCs_WorkP


# Back to the original graphics device
par(mfrow = c(1, 1))


#iceplot_NCCs_Parks +iceplot_NCCs_GP+iceplot_NCCs_TS
#********************************************************************************************************** 


