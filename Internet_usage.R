#install.packages("tidyverse")
library(tidyverse)
# library(rmarkdown)
#install.packages("devtools")
# devtools::install_github("cardiomoon/ggiraphExtra")
# library(ggiraph)
# library(ggiraphExtra)

# Data Wrangling

# Data 
# - CIA World Fact Book 2017
# - Democracy Index 2017
# - Wikipedia: Regional Classifications

# Internet
internetusers <- read_csv("Data/internetusers_cia2017.csv")
internetusers <- internetusers[c(2,3)]
colnames(internetusers) <- c("Country","Users")

# Population
population <- read_csv("Data/population_cia2017.csv")
population <- population[c(2,3)]
colnames(population) <- c("Country","Population")

# Internet usage
usage <- inner_join(internetusers, population) 
usage <- mutate(usage, Percent = usage$Users / usage$Population)

# Region (unmodified)
region <- read_csv("Data/world_regions.csv")

# Democracy
democracy <- read_csv("Data/democracyindex2017.csv")
democracy <- democracy [-c(21, 78, 118, 171),]
democracy <- democracy [c(2,3,9)]
democracy$Category <- as.factor(democracy$Category)
democracy$Score <- as.numeric(democracy$Score)

# Education
education <- read_csv("Data/education_cia2017.csv")
education <- education[c(2, 3)]
colnames(education) <- c("Country", "EduExpend")

# Economy
gdpppp <- read_csv("Data/gdpppp_cia2017.csv")
gdpppp <- gdpppp[c(2, 3)]
colnames(gdpppp) <- c("Country","GDP")
gdpppp$GDP <- as.numeric(gsub('[$,]', '', gdpppp$GDP))

# Health
lifeexpect <- read_csv("Data/lifeexpect_cia2017.csv")
lifeexpect <- lifeexpect[c(2,3)]
colnames(lifeexpect) <- c("Country", "LifeExpect") 
lifeexpect <- mutate(lifeexpect, AgeGroup = cut(lifeexpect$LifeExpect, c(50, 60, 70, 80, 90)))

healthexpend <- read_csv("Data/healthexpend_cia2017.csv")
healthexpend <- healthexpend[c(2,3)]
colnames(healthexpend) <- c("Country", "HealthExpend")

# internet & democracy
internet_demo <- inner_join(usage, democracy)
  # plot
internet_demo %>% ggplot(aes(x=Score, y=Percent)) + geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=TRUE)
  # linear regression 
lm_demo <- lm(Percent ~ Score, internet_demo)
summary(lm_demo)
  # catagorized
internet_demo %>% ggplot(aes(x=Score, y=Percent, color=Category)) + geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=TRUE)

# internet & economy
internet_econ <- inner_join(usage, gdpppp)
  # plot
internet_econ %>% ggplot(aes(x=GDP, y=Percent)) + geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=TRUE)
  # linear regression
lm_econ <- lm(Percent ~ GDP, internet_econ)
summary(lm_econ)

# internet & education
internet_edu <- inner_join(usage, education) %>%
  inner_join(., gdpppp) %>%
  mutate(RealEdu = GDP * EduExpend * 0.01)
  #plot
internet_edu %>% ggplot(aes(x=RealEdu, y=Percent)) + geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=TRUE)
  #linear regression
lm_edu <- lm(Percent ~ RealEdu, internet_edu)
summary(lm_edu)

# internet & health
internet_health <- inner_join(usage, healthexpend) %>%
  inner_join(., lifeexpect) %>%
  inner_join(., gdpppp) %>%
  mutate(RealHealth = HealthExpend * GDP * 0.01)
internet_health <- internet_health[c(1, 4, 6, 7, 9)]
  #linear regression
lm_health <- lm(Percent ~ RealHealth + LifeExpect, internet_health)
summary(lm_health)
  #plot
internet_health %>% ggplot(aes(x=Percent, y=RealHealth, color = LifeExpect)) + geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=TRUE)
  #plot with ggPredict
ggPredict(lm_health, se=TRUE)

  #categorized
internet_health %>% ggplot(aes(x=Percent, y=RealHealth, color = AgeGroup)) + geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=TRUE)