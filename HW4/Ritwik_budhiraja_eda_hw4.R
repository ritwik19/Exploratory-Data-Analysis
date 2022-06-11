# RITWIK BUDHIRAJA | 2000916462 | rbudhira@iu.edu

library(tidyverse)
library(broom)
library(GGally)
library(viridis)
data = read.table("/Users/ritwikbudhiraja/Desktop/Indiana/SEM_2/EDA/HW4/movie_budgets.txt",sep=" ",header=T)
data = as_tibble(data)
head(data)

# QUESTION 1:

# Correlation between different variables
ggpairs(data %>%
          select(year, length, budget))

# Graph between the log10(budget) and the release years of the movies
ggplot(data) +
  geom_point(aes(x = year, y = log10(budget)), size = 0.2) +
  ggtitle("Graph between the log10(budget) and the release years of the movies") +
  xlab("Release years of the movies") +
  ylab("Log10(Budget of the movies)")

#Linear regression fitted graph between the log10(budget) and the year of the movies
LinReg_year = lm(log10(budget) ~ year,
                  data = data)
ggplot(data) +
  geom_point(aes(x = year, y = log10(budget)), size = 0.2) +
  geom_line(aes(x = year, y = .fitted),
            data = augment(LinReg_year, data = data), size = 1, color = "blue") +
  ggtitle("Linear regression fitted graph between the log10(budget) and the year of the movies") +
  xlab("Release years of the movies") +
  ylab("Log10(Budget of the movies)")
  
#Polynomial regression (degree = 2) fitted graph between the log10(budget) and the year of the movies
PolyReg_year = lm(log10(budget) ~ year + I(year^2),
                  data = data)
ggplot(data) +
  geom_point(aes(x = year, y = log10(budget)), size = 0.2) +
  geom_line(aes(x = year, y = .fitted),
            data = augment(PolyReg_year, data = data), size = 1, color = "blue") +
  ggtitle("Polynomial regression fitted graph between the log10(budget) and the year of the movies") +
  xlab("Release years of the movies") +
  ylab("Log10(Budget of the movies)")

# Loess fitted graph between the log10(budget) and the release years of the movies
loess_year = loess(log10(budget) ~ year, span = 0.4, degree = 2, data = data, family = "symmetric")
ggplot(data) +
  geom_point(aes(x = year, y = log10(budget)), size = 0.2) +
  geom_line(aes(x = year, y = .fitted),
            data = augment(loess_year, data = data), size = 1, color = "red") +
  ggtitle("Loess fitted graph between the log10(budget) and the release years of the movies") +
  xlab("Release years of the movies") +
  ylab("Log10(Budget of the movies)")

# Graph between the log10(budget) and the length of the movies
ggplot(data) +
  geom_point(aes(x = length, y = log10(budget)), size = 0.2) +
  ggtitle("Graph between the log10(budget) and the length of the movies") +
  xlab("Length of the movies") +
  ylab("Log10(Budget of the movies)")

# Linear regression fitted graph between the log10(budget) and the length of the movies
LinReg_length = lm(log10(budget) ~ length,
                    data = data)
ggplot(data) +
  geom_point(aes(x = length, y = log10(budget)), size = 0.2) +
  geom_line(aes(x = length, y = .fitted),
            data = augment(LinReg_length, data = data), size = 1, color = "blue") +
  ggtitle("Linear regression fitted graph between the log10(budget) and the length of the movies") +
  xlab("Length of the movies") +
  ylab("Log10(Budget of the movies)")

# Polynomial regression (degree = 2) fitted graph between the log10(budget) and the length of the movies
PolyReg_length = lm(log10(budget) ~ length + I(length^2),
                    data = data)
ggplot(data) +
  geom_point(aes(x = length, y = log10(budget)), size = 0.2) +
  geom_line(aes(x = length, y = .fitted),
            data = augment(PolyReg_length, data = data), size = 1, color = "blue") +
  ggtitle("Polynomial regression fitted graph between the log10(budget) and the length of the movies") +
  xlab("Length of the movies") +
  ylab("Log10(Budget of the movies)")

# Loess fitted graph between the log10(budget) and the length of the movies
loess_length = loess(log10(budget) ~ length, span = 0.5, degree = 2, data = data, family = "symmetric")
ggplot(data) +
  geom_point(aes(x = length, y = log10(budget)), size = 0.2) +
  geom_line(aes(x = length, y = .fitted),
            data = augment(loess_length, data = data), size = 1, color = "red") +
  ggtitle("Loess fitted graph between the log10(budget) and the length of the movies") +
  xlab("Length of the movies") +
  ylab("Log10(Budget of the movies)")



#QUESTION 2:

# Some Exploratory Data Analysis
min(data$year)
max(data$year)
min(data$length)
max(data$length)
length(unique(data$length))
length(unique(data$year))

# Loess fitted graph between the log10(budget) and the length of the movies faceted on the release years
ggplot(data, aes(x = length, y = log10(budget))) + 
  geom_point(size = 0.2) +
  facet_wrap(~ cut_number(year, n = 8), ncol = 4, scales = "free") +
  ylab("Log10(Budget of the movie)") +
  xlab("Length of the movies") +
  geom_smooth(method = "loess", se = FALSE, span = 0.5, method.args = list(degree = 2, family = "symmetric")) +
  ggtitle("Loess fitted graph between the log10(budget) and the length of the movies faceted on the release years")

# Loess fitted graph between the log10(budget) and the release years faceted on the length of the movies
ggplot(data, aes(x = year, y = log10(budget))) + 
  geom_point(size = 0.2) +
  facet_wrap(~ cut_number(length, n = 10), ncol = 4, scales = "free") +
  ylab("Log10(Budget of the movie)") +
  xlab("Release years") +
  geom_smooth(method = "loess", se = FALSE, span = 0.4, method.args = list(degree = 2, family = "symmetric")) +
  ggtitle("Loess fitted graph between the log10(budget) and the release years faceted on the length of the movies")


# QUESTION 3

# Creating x and y dimensions
movie_grid = data.frame(expand.grid(
  length = seq(1, 390, 1),
  year = seq(1906, 2005, 1)))

# Building a loess model between the budget and the length, release year of the movies
loess_ques3 = loess(log10(budget) ~ year * length,
                     data = data, span = 0.5, degree = 2,
                     family = "symmetric", normalize = FALSE)

loess_contour = augment(loess_ques3, newdata = movie_grid)

ggplot(loess_contour, aes(x = year, y = length, fill = .fitted, z = .fitted)) +
  geom_raster() +
  geom_contour(bins = 30, color = "black") +
  coord_fixed() +
  scale_fill_viridis("Fitted movie budget") +
  ylab("Length of the movies") +
  xlab("Release years") +
  ggtitle("Contour plot depicting the relationship between the budget and the length, release year of the movies")









