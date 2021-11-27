##################
## Assignment 1 ##
##################

# clean environtment

rm(list=ls())

# install packages
library(tidyverse)
library(lspline)
library(cowplot)
library(boot)
library(estimatr)
library(huxtable)
library(stargazer)
library(modelsummary)
library(gridExtra)

# load data
getwd()

df <- read_csv("https://osf.io/4ay9x/download")

View(df)

# Generate variable for females, hourly wages, log hourly wages and levels of education
df <- df %>% mutate(female=as.numeric(sex==2)) %>%
  mutate(w=earnwke/uhours) %>%
  mutate(lnw=log(w)) %>% 
  mutate(ed_BA=as.numeric(grade92==43),
         ed_MA=as.numeric(grade92==44),
         ed_Prof = as.numeric(grade92==45),
         ed_PhD = as.numeric(grade92==46))

# Create gender variable with values "Male" and "Female"
df$gender <- as.numeric(df$sex)
df$gender[df$gender==1] <- "Male"
df$gender[df$gender==2] <- "Female"
df$gender <- as.character(df$gender)


#Filter for Financial Specialists, weekly working hours >20, age between 24-64 and level of education starting from BA degrees
data_fa <- df %>% filter (occ2012>=0800 & occ2012<=0950 & uhours>=20 & earnwke>0 & age>=24 & age<=64 & grade92>42)


# Show distribution of earnings

data_fa %>% dplyr::select(earnwke,uhours,w) %>% summary()

data_fa %>% filter(w>=1) %>% dplyr::select(earnwke,uhours,w) %>% summary()

# Show distribution of females/males among Financial specialists
tabulate(data_fa$sex)

# Examine the distribution of wage and log wage
dist_w<- ggplot(data = data_fa, aes(x=w, color=gender))+
  geom_density() +
  labs(title="Wage distribution", x="Hourly wage", y="Ratio")

dist_w

dist_lnw <- ggplot(data = data_fa, aes(x=lnw, color=gender))+
  geom_density()+
  labs(title="Log Wage distribution", x=" Log of hourly Wage", y="Ratio")

dist_lnw

# unconditional gendergap
reg0 <- lm(lnw~female,data_fa) 
summary(reg1)

reg1 <- lm_robust(lnw ~ female, data = data_fa, se_type = "HC1")
summary(reg2)                            

reg2 <- lm_robust(w ~ female, data = data_fa, se_type = "HC1")

msummary(list("Log wage"= reg1, "Wage" = reg2),
         fmt="%.4f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         title="Unconditional regression")

## From the log-level regression, we can be 95% confident that female Financial specialists earned 17.99-22.61% less, on average, than men in the USA in 2014.
## From the level-level model, we can be 95% fonfident that female Financial specialists earned 5.94-7.22 dollars less per hour, on average, than men in the USA in 2014.


# Taking education level into consideration
reg3 <- lm_robust(w ~ female + ed_MA + ed_Prof + ed_PhD, data=data_fa, se_type = "HC1")
reg4 <- lm_robust(w ~ ed_MA*female + ed_Prof*female + ed_PhD*female, data=data_fa, se_type = "HC1")

#BA is the reference degree in regression 3 and 4.

msummary(list(" Wage - Female "= reg2, "Wage - level of education" = reg3, "Wage - level of education x female" = reg4),
         fmt="%.4f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01))

# Comparing employees of the same gender, those with an MA degree earned 3.60, those with a Professional degree 6.83, and those with a PhD 11.58 dollars per hour more than employees with a BA degree (at a 1% significance level).
# When applying level of education interacted with gender, the hourly wage difference between females with a BA and females with an MA degree are not significant. However, the hourly wage difference between females with a Professional degree and females with PhD is significant at 95%. Thus, we can conclude that females with MA degree earned 10.27 dollars, whereas females with PhD degree earned 18.33 dollars more, on average, with females with BA degree.

## Estimating regressions separately for men and women
f_data_fa <- data_fa %>% filter(female==1)
m_data_fa <- data_fa %>% filter(female==0)

reg5 <- lm_robust(w ~   ed_MA + ed_Prof + ed_PhD, data=f_data_fa, se_type = "HC1")

reg6 <- lm_robust(w ~  ed_MA+ ed_Prof + ed_PhD, data = m_data_fa, se_type = "HC1")

reg7 <- lm_robust(w ~ female + ed_MA*female + ed_Prof*female + ed_PhD*female, data = data_fa, se_type = "HC1")

msummary(list("Women"= reg5, "Men" = reg6, "All" = reg7),
         fmt="%.4f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01))

## Visualization of the relationship between the level of education and the log of earnings per hour (for females and males)
p1 <- ggplot(data = data_fa, aes(x = grade92, y = lnw, col=gender)) +
  geom_point() + 
  geom_smooth(method="lm") +
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(42.5, 46.5),   breaks=seq(38, 65,   by=1)) + 
  scale_y_continuous(expand=c(0.01, 0.01),limits = c(1.5, 4.5), breaks=seq(1.5, 4.5, by=0.50)) +
  facet_grid(~gender) +
  labs(title="Relationship between the level of education and log wages per hour",x = "Level of education",y = "Log of hourly wage")

p1

## Visualization of the relationship between the level of education and the earnings per hour (for females and males)

p2 <- ggplot(data = data_fa, aes(x = grade92, y = w, col=gender)) +
  geom_point() + 
  geom_smooth(method="lm") +
  facet_grid(~gender) +
  labs(title="Relationship between the level of education and log wages per hour",x = "Level of education",y = "Hourly wage")

p2







