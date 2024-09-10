otu <- read.delim('dep.txt', row.names = 1)
plant <- read.delim('depgroup.txt', row.names = 1)
otu <- data.frame(t(otu))
otu <- otu[rownames(plant), ]
otu <- cbind(otu, plant)
set.seed(123)
train <- sample(nrow(otu), nrow(otu)*0.7)
otu_train <- otu[train, ]
otu_test <- otu[-train, ]
library(randomForest)
set.seed(123)
otu_train.forest <- randomForest(plant_age~., data = otu_train, importance = TRUE)
otu_train.forest
plant_predict <- predict(otu_train.forest, otu_train)
plot(otu_train$plant_age, plant_predict, main = '训练集', 
    xlab = 'Plant age (days)', ylab = 'Predict')
abline(1, 1)
plant_predict <- predict(otu_train.forest, otu_test)
plot(otu_test$plant_age, plant_predict, main = '测试集',
    xlab = 'Plant age (days)', ylab = 'Predict')
abline(1, 1)
importance_otu <- otu_train.forest$importance
head(importance_otu)
importance_otu <- data.frame(importance(otu_train.forest), check.names = FALSE)
head(importance_otu)
varImpPlot(otu_train.forest, n.var = min(10, nrow(otu_train.forest$importance)), 
    main = 'Top 10 - variable importance')
importance_otu <- importance_otu[order(importance_otu$IncNodePurity, decreasing = TRUE), ]
head(importance_otu)
set.seed(123)
otu_train.cv <- replicate(5, rfcv(otu_train[-ncol(otu_train)], otu_train$plant_age, cv.fold = 10, step = 1.5), simplify = FALSE)
otu_train.cv
otu_train.cv <- data.frame(sapply(otu_train.cv, '[[', 'error.cv'))
otu_train.cv$otus <- rownames(otu_train.cv)
otu_train.cv <- reshape2::melt(otu_train.cv, id = 'otus')
otu_train.cv$otus <- as.numeric(as.character(otu_train.cv$otus))
otu_train.cv.mean <- aggregate(otu_train.cv$value, by = list(otu_train.cv$otus), FUN = mean)
head(otu_train.cv.mean, 10)
library(ggplot2)
ggplot(otu_train.cv.mean, aes(Group.1, x)) +
geom_line() +
theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +  
labs(title = '',x = 'Number of OTUs', y = 'Cross-validation error')
importance_otu <- importance_otu[order(importance_otu$IncNodePurity, decreasing = TRUE), ]
importance_otu.select <- importance_otu[1:10, ]
importance_otu.select
write.table(importance_otu.select, 'importance_otu.select.txt', sep = '/t', col.names = NA, quote = FALSE)
otu_id.select <- rownames(importance_otu.select)
otu.select <- otu[ ,c(otu_id.select, 'plant_age')]
otu.select <- reshape2::melt(otu.select, id = 'plant_age')

ggplot(otu.select, aes(x = plant_age, y = value)) +
geom_point() +
geom_smooth() +
facet_wrap(~variable, ncol = 3, scale = 'free_y') +
labs(title = '',x = 'hypoxia', y = 'expression')
otu.select <- otu[ ,c(otu_id.select, 'plant_age')]
set.seed(123)
train <- sample(nrow(otu.select), nrow(otu.select)*0.7)
otu_train.select <- otu.select[train, ]
otu_test.select <- otu.select[-train, ]
set.seed(123)
otu_train.select.forest <- randomForest(plant_age~., data = otu_train.select, importance = TRUE)
otu_train.select.forest
plant_predict <- predict(otu_train.select.forest, otu_train.select)

plot(otu_train.select$plant_age, plant_predict, main = '训练集', 
    xlab = 'Plant age (days)', ylab = 'Predict')
abline(1, 1)

plant_predict <- predict(otu_train.select.forest, otu_test.select)

plot(otu_test.select$plant_age, plant_predict, main = '测试集',
    xlab = 'Plant age (days)', ylab = 'Predict')
abline(1, 1)
varImpPlot(otu_train.forest, n.var = min(10, nrow(otu_train.forest$importance)), 
           main = 'Top 10 - variable importance')

