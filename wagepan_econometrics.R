#remove old objects
rm(list = ls())

#packages
library(haven)
library(lmtest)
library(sandwich)
library(plm)


wagepan<-read.csv("C:/Users/haiva/OneDrive/Desktop/wagepan.csv")
print(wagepan)
str(wagepan)
summary(wagepan)
head(wagepan)

#generate variable
wagepan$expersq<-wagepan$exper^2

#variations overtime for first 3 workers
coplot(lwage~year|factor(nr),data=wagepan[wagepan$nr<=18,],
      show.given = FALSE, columns = 3, pch=16, col = "Red" )


sum(is.na(wagepan))

wagepan <- na.omit(wagepan)
summary(wagepan)

wagepan$nr<-log(wagepan$year)
summary(wagepan$nr)

wagepan$agric<-log(wagepan$black)
summary(wagepan$agric)

wagepan$hisp<-log(wagepan$married)
summary(wagepan$hisp)

library(dplyr)

# Check variability within individuals for selected variables
time_variation <- wagepan %>%
  group_by(nr) %>%
  summarize(
    year_range = length(unique(year)),        # Should vary over time
    exper_range = max(exper) - min(exper),    # Should vary over time
    union_range = length(unique(union))       # Should vary if union status changes
  )
print(time_variation)

# Check variability across individuals for selected variables
individual_variation <- wagepan %>%
  group_by(nr) %>%
  summarize(
    educ_unique = length(unique(educ)),  # Should be constant for each individual
    black_unique = length(unique(black)),
    hisp_unique = length(unique(hisp))
  )
print(individual_variation)

library(ggplot2)

ggplot(wagepan, aes(x = year, y = exper, color = as.factor(nr))) +
  geom_line() +
  labs(title = "Labor Market Experience Over Time", x = "Year", y = "Experience") +
  theme_minimal()

ggplot(wagepan, aes(x = nr, y = educ)) +
  geom_point() +
  labs(title = "Education Across Individuals", x = "Individual ID", y = "Education") +
  theme_minimal()

wagepan$year <- as.factor(wagepan$year)
wagepan$nr <- as.factor(wagepan$nr)
wagepan$black <- as.factor(wagepan$black)
wagepan$hisp <- as.factor(wagepan$hisp)
wagepan$married <- as.factor(wagepan$married)
wagepan$union <- as.factor(wagepan$union)



pooled_ols <- lm('lwage ~ educ + black + hisp + exper + I(exper^2) + married + union', data = wagepan)
summary(pooled_ols)


X <- model.matrix(~ educ + black + hisp + exper + I(exper^2) + married + union, data = wagepan)
summary(X)
y <- wagepan$lwage
pooled_ols <- lm(y ~ X)
summary(pooled_ols)


library(plm)
wagepan <- pdata.frame(wagepan, index = c("nr", "year"))
pooled_ols <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union, 
                  data = wagepan, 
                  model = "pooling")
summary(pooled_ols)

library(sandwich)
library(lmtest)
coeftest(pooled_ols, vcov = vcovHC(pooled_ols, type = "HC1"))


library(plm)
random_effects <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union, 
                      data = wagepan, 
                      index = c("nr", "year"), 
                      model = "random")
summary(random_effects)

coeftest(random_effects, vcov = vcovHC(random_effects, type = "HC1"))

plm(formula = lwage ~ I(exper^2) + married + union + educ + factor(year) + 
        factor(year):educ, data = wagepan, model = "within", index = c("nr", 
        "year"))
plm(formula = lwage ~ educ + black + hisp + exper + I(exper^2) + 
         married + union, data = wagepan, model = "pooling", index = c("nr", 
         "year"))

