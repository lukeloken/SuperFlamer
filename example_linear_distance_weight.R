
library(dplyr)

#Example flame dataset

set.seed(11)
dist <- c(1,3,3,4,7,9)
x1 <- runif(6, 100, 10000)
x2 <- runif(6, 1, 14)

example_df <- data.frame(dist, x1, x2)


#Generate prediction data.frame
empty_df <- example_df %>%
  mutate(across(2:ncol(example_df), ~ NA))

predict_df <- data.frame(dist = 1:10) %>%
  left_join(empty_df) %>%
  distinct() %>%
  arrange(dist)

rm(empty_df)

#Generate a distance matrix for all predicted locations
dist_matrix <- as.matrix(stats::dist(predict_df$dist, upper = TRUE))

#make a rectangle that is the distance between all observations (rows) and predictions (cols)
#note that some rows get duplicated, others get dropped based on flame dataset
dist_rect <- dist_matrix[example_df$dist,]

#Replace zeros with a small number. Note that this value has not been tested
dist_rect[which(dist_rect == 0)] <- 1

#Calculate inverse distance, can change power if you wish
power = 1
inv_dist_rect <- 1/(dist_rect^power)

col_nu = 2
for (col_nu in 2:ncol(example_df)){
  
  #observation values
  obs_i <- example_df[,col_nu]
  #locations to predict
  dist_i <- predict_df$dist
  
  #apply prediction distance to custom function 
  # example for first value in dist
  # d = dist_i[1]
  # e <- weighted.mean(obs_i, inv_dist_rect[,d])

  #use apply function to calculate weighted mean at each location
  preds_i <- sapply(dist_i, function(d) weighted.mean(obs_i, inv_dist_rect[,d]))
  
  #save to output data.frame
  predict_df[,col_nu] <- preds_i
  

}

predict_df

library(ggplot2)
ggplot(predict_df, aes(x = dist, y = x1)) +
  geom_path() +
  geom_point(data = example_df, col = "red", shape = 8) +
  ggtitle(paste("Power =", power))

ggplot(predict_df, aes(x = dist, y = x2)) +
  geom_path() +
  geom_point(data = example_df, col = "red", shape = 8) +
  ggtitle(paste("Power =", power))
