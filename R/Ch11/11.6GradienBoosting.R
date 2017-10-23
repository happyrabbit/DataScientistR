#########
# 助推法
#########

gbmGrid <- expand.grid(interaction.depth = c(1, 3, 5, 7, 9),
                       n.trees = 1:5,
                       shrinkage = c(.01, .1),
                       n.minobsinnode = c(1:10))

set.seed(100)
gbmTune <- train(x = trainx, 
                y = trainy,
                method = "gbm",
                tuneGrid = gbmGrid,
                metric = "ROC",
                verbose = FALSE,
                trControl = trainControl(method = "cv",
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           savePredictions = TRUE))

treebagRoc <- roc(response = bagTune$pred$obs,
                        predictor = bagTune$pred$Female,
                        levels = rev(levels(bagTune$pred$obs)))
rfRoc <- roc(response = rfTune$pred$obs,
             predictor = rfTune$pred$Female,
             levels = rev(levels(rfTune$pred$obs)))
gbmRoc <- roc(response = gbmTune$pred$obs,
              predictor = gbmTune$pred$Female,
              levels = rev(levels(gbmTune$pred$obs)))
plot(rpartRoc, type = "s", print.thres = c(.5),
     print.thres.pch = 16,
     print.thres.pattern = "",
     print.thres.cex = 1.2,
     col = "black", legacy.axes = TRUE,
     print.thres.col = "black")
     
plot(treebagRoc, type = "s", add = TRUE, print.thres = c(.5), 
     print.thres.pch = 3, legacy.axes = TRUE, print.thres.pattern = "", 
     print.thres.cex = 1.2,
     col = "red", print.thres.col = "red")
 
plot(rfRoc, type = "s", add = TRUE, print.thres = c(.5), 
     print.thres.pch = 1, legacy.axes = TRUE, print.thres.pattern = "", 
     print.thres.cex = 1.2,
     col = "green", print.thres.col = "green")

plot(gbmRoc, type = "s", add = TRUE, print.thres = c(.5), 
     print.thres.pch = 10, legacy.axes = TRUE, print.thres.pattern = "", 
     print.thres.cex = 1.2,
     col = "blue", print.thres.col = "blue")
     
legend(0.2, 0.5, cex = 0.8,
       c("Single Tree", "Bagged Tree", "Random Forest", "Boosted Tree"),
       lwd = c(1, 1, 1, 1),
       col = c("black", "red", "green", "blue"),
       pch = c(16, 3, 1, 10))




