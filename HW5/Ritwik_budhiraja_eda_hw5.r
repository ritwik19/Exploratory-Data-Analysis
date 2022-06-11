# RITWIK BUDHIRAJA | 2000916462 | rbudhira@iu.edu


library(reshape)
library(ggplot2)
library(tidyverse)
library(broom)
library(viridis)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(ggrepel)
load("/Users/ritwikbudhiraja/Desktop/Indiana/SEM_2/EDA/HW3/lattice.RData")
head(hamster)
hamster = as_tibble(hamster)


## QUESTION 1

# PCA with the un-standardized variables
hamster_unstd = prcomp(hamster, scale. = FALSE)
ggbiplot(hamster_unstd, scale = 1) + 
  ggtitle("PCA biplot of Hamster with the un-standardized variables") + 
  xlab("Principal component 1") + 
  ylab("Principal component 2") 

# PCA with the standardized variables
hamster_std = prcomp(hamster, scale. = TRUE)
ggbiplot(hamster_std, scale = 1) + 
  ggtitle("PCA biplot of Hamster with the standardized variables") + 
  xlab("Principal component 1") + 
  ylab("Principal component 2")


## QUESTION 2

nyt = read_csv("http://jfukuyama.github.io/teaching/stat670/assignments/nyt_articles.csv")
length(unique(nyt$class.labels))

# Removing the first column because it's a non numeric column
nyt_test = nyt[,2:4432]
# Filtering the dataframe to remove the columns with 0 variance
nyt_q2 = nyt_test[,apply(nyt_test, 2, var, na.rm=TRUE) != 0]

# PCA with the un-standardized variables
nyt_unstd = prcomp(nyt_test, scale. = FALSE)
ggbiplot(nyt_unstd, scale = 1, groups = nyt$class.labels, var.axes = FALSE) + 
  ggtitle("PCA biplot of NYT articles with the un-standardized variables") + 
  xlab("Principal component 1") + 
  ylab("Principal component 2") +
  guides(col=guide_legend("Article type"))


# PCA with the standardized variables
nyt_std = prcomp(nyt_q2, scale. = TRUE)
ggbiplot(nyt_std, scale = 1, groups = nyt$class.labels, var.axes = FALSE) + 
  ggtitle("PCA biplot of NYT articles with the standardized variables") + 
  xlab("Principal component 1") + 
  ylab("Principal component 2") +
  guides(col=guide_legend("Article type"))


## QUESTION 3

nyt_rot = data.frame(nyt_unstd$rotation[,1:2])
nyt_unstd_df = data.frame(nyt_unstd$x)
nyt_unstd_df_cl = mutate(nyt_unstd_df, class.labels=nyt$class.labels)
  
# Projecting top 7 variables
nyt_rot_pc1 = nyt_rot[order(-nyt_rot$PC1),][1:7,]
nyt_rot_pc2 = nyt_rot[order(-nyt_rot$PC2),][1:7,]

# PCA biplot of NYT articles of top 7 variables ordered over PC1
ggplot(nyt_unstd_df_cl) +
  geom_point(aes(x=PC1, y=PC2, color=class.labels)) +
  geom_segment(data=nyt_rot_pc1, aes(xend=PC1, yend=PC2, x=0, y=0), arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text_repel(data=nyt_rot_pc1, aes(x=PC1, y=PC2, label=row.names(nyt_rot_pc1))) + 
  ggtitle("PCA biplot of NYT articles of top 7 variables ordered over PC1") + 
  xlab("Principal component 1") + 
  ylab("Principal component 2") +
  guides(col=guide_legend("Article type"))

# PCA biplot of NYT articles of top 7 variables ordered over PC2
ggplot(nyt_unstd_df_cl) +
  geom_point(aes(x=PC1, y=PC2, color=class.labels)) +
  geom_segment(data=nyt_rot_pc2, aes(xend=PC1, yend=PC2, x=0, y=0), arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text_repel(data=nyt_rot_pc2, aes(x=PC1, y=PC2, label=row.names(nyt_rot_pc2))) + 
  ggtitle("PCA biplot of NYT articles of top 7 variables ordered over PC2") + 
  xlab("Principal component 1") + 
  ylab("Principal component 2") +
  guides(col=guide_legend("Article type"))

