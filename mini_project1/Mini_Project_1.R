library(tidyverse)
library(broom)
# install.packages("gapminder")
library(gapminder)

# Question 1:

# Plot for GDP per capita vs Life Expectancy.
ggplot(gapminder) + 
  geom_point(aes(x = gdpPercap, y = lifeExp)) +
  ylab("Life Expectancy") +
  xlab("GDP per Capita") +
  ggtitle("Life Expectancy vs GDP per Capita")

# Sub-setting the data for the year 2007.
GDP_vs_LifeExp = subset(gapminder, year == 2007)

# Plot for GDP per capita vs Life Expectancy in the year 2007.
ggplot(GDP_vs_LifeExp) + 
  geom_point(aes(x = gdpPercap, y = lifeExp)) +
  ylab("Life Expectancy") +
  xlab("GDP per Capita") +
  ggtitle("Life Expectancy vs GDP per Capita for the year 2007")

# Box-cox tranformation function.
boxcox = function(y , tau) {
  return((y^tau - 1)/tau)
}

# Boxcox transformed (Tau = 3) plot for Life Expectancy vs GDP per Capita for the year 2007
ggplot(GDP_vs_LifeExp) + 
  geom_point(aes(x = gdpPercap, y = boxcox(lifeExp,3))) +
  ylab("Life Expectancy") +
  xlab("GDP per Capita") +
  ggtitle("Boxcox transformed (Tau = 3) plot for Life Expectancy vs GDP per Capita for the year 2007")


# Polynomial Regression of exponent 3 between the variables GDP per capita and life expectancy.
PolyReg = lm(lifeExp ~ gdpPercap + I(gdpPercap^2) + I(gdpPercap^3),
             data = gapminder, subset = year == 2007) 

# Visualizing the polynomial and the LOESS models.
ggplot(augment(PolyReg),aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  geom_line(aes(x = gdpPercap, y = .fitted, color = "Polynomial Regression")) +
  geom_smooth(method = 'loess', span = .75, method.args = list(degree = 2), aes(color = "Local Regression"))  +
  ylab("Life Expectancy") +
  xlab("GDP per Capita") +
  ggtitle("Life Expectancy vs GDP per Capita fitted model for the year 2007") +
  scale_color_manual(name = "Regression models",             # legend name
                     values = c("Polynomial Regression" = "red",  # map regression line colors
                                "Local Regression" = "blue"))

# Visualizing the polynomial and the LOESS models for each continent.
ggplot(augment(PolyReg),aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  geom_line(aes(x = gdpPercap, y = .fitted, color = 'Polynomial Regression')) +
  geom_smooth(method = 'loess', span = .75, method.args = list(degree = 2), aes(color = "Local Regression")) +
  facet_wrap(~GDP_vs_LifeExp$continent, scales="free")  +
  ylab("Life Expectancy") +
  xlab("GDP per Capita") +
  ggtitle("Life Expectancy vs GDP per Capita fitted model for the year 2007 for each Continent") +
  scale_color_manual(name = "Regression models",             # legend name
                     values = c("Polynomial Regression" = "red",  # map regression line colors
                                "Local Regression" = "blue"))

# LOESS curve between the variables GDP per capita and life expectancy.
smoother_poly = loess(lifeExp ~ gdpPercap, data = gapminder, subset = year == 2007, span = 0.75, degree = 2)

# Visualizing the above LOESS curve.
ggplot(augment(smoother_poly)) + geom_point(aes(x = gdpPercap, y = lifeExp)) +
  geom_line(aes(x = gdpPercap, y = .fitted), color = 'red') +
  xlab("GDP per Capita") + ylab("Life expectancy") + 
  ggtitle("LOESS curve between GDP per Capita and Life expectancy")



# Question 2

# Plot for average life expectancy over time for each continent
gapminder %>% 
  group_by(continent, year) %>% 
  summarise(lifeExp=mean(x=lifeExp)) %>% 
  ggplot(aes(x=year, y=lifeExp, colour=continent)) + geom_line() + geom_point() + 
  xlab("Year") + ylab("Average Life expectancy") + 
  ggtitle("Average life expectancy over time for each continent")

# Plot for Life Expectancy of each country in Americas and Asia

gapminder %>% 
  filter(continent==c("Asia", "Americas", "Africa", "Europe")) %>%
  ggplot(aes(x=year, y=lifeExp, group=country, color=country)) +
  geom_line(lwd = 1, show.legend=F) +
  facet_wrap(~continent, scales = 'free') +
  scale_color_manual(values=country_colors) +
  xlab("Year") + ylab("Life Expectancy") + 
  ggtitle("Life Expectancy trends in each country faceted over Continents")


# Question 3:

# Gapminder data grouped by year and mean of life expectancy, GDP.
ques3 = gapminder %>%
  group_by(year) %>%
  mutate(group_life_exp = mean(lifeExp),
         group_gdp = mean(gdpPercap))

head(ques3)

# Gapminder data grouped by year, continent and mean of life expectancy, GDP.
ques3_facet = gapminder %>%
  group_by(year, continent) %>%
  mutate(group_life_exp = mean(lifeExp),
         group_gdp = mean(gdpPercap))

head(ques3_facet)


# Polynomial model of degree 3 fitted over the above data.
PolyReg_q3_a = lm(group_life_exp ~ group_gdp + I(group_gdp^2) + I(group_gdp^3),
             data = ques3)

# Plotting various linear models over the data.
ggplot(augment(PolyReg_q3_a),aes(x = group_gdp, y = group_life_exp)) + 
  geom_point() +
  geom_line(aes(x = group_gdp, y = .fitted, color = "Polynomial Regression")) +
  geom_smooth(method = 'loess', span = .75, method.args = list(degree = 2), aes(color = "Local Regression"))  +
  ylab("Average Life Expectancy") +
  xlab("Average GDP per Capita") +
  ggtitle("Average Life Expectancy vs average GDP per Capita models") +
  scale_color_manual(name = "Regression models",             # legend name
                     values = c("Polynomial Regression" = "red",  # map regression line colors
                                "Local Regression" = "blue"))


# Polynomial model of degree 3 fitted over the data for each continent.
PolyReg_q3_b = lm(group_life_exp ~ group_gdp + I(group_gdp^2) + I(group_gdp^3),
                  data = ques3_facet)

# Plotting various linear models over the data for each continent.
ggplot(augment(PolyReg_q3_b),aes(x = group_gdp, y = group_life_exp)) + 
  geom_point() +
  geom_line(aes(x = group_gdp, y = .fitted, color = "Polynomial Regression")) +
  geom_smooth(method = 'loess', span = .75, method.args = list(degree = 2), aes(color = "Local Regression"))  +
  facet_wrap(~ques3_facet$continent, scales = "free")  +
  ylab("Average Life Expectancy") +
  xlab("Average GDP per Capita") +
  ggtitle("Average Life Expectancy vs average GDP per Capita fitted model faceted by Continents") +
  scale_color_manual(name = "Regression models",             # legend name
                     values = c("Polynomial Regression" = "red",  # map regression line colors
                                "Local Regression" = "blue"))

# correlation matrix of plots
ggpairs(gapminder %>%
          select(year, lifeExp, gdpPercap))

# Loess plot of Life Expectancy vs GDP per Capita faceted by intervals of years and colored by Continents
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(size = 0.2) +
  facet_wrap(~ cut_number(year, n = 7), ncol = 4, scales = "free") +
  ylab("Life Expectancy") +
  xlab("GDP per Capita") +
  geom_smooth(method = "loess", se = FALSE, span = 1, method.args = list(degree = 2), aes(color = continent)) +
  ggtitle("Life Expectancy vs GDP per Capita Loess fitted model faceted by intervals of years and colored by Continents") +
  guides(color = guide_legend(title = "Continents")) 
