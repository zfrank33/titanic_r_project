library(tidyverse) 

# Read in the data from a CSV file
titanic <- read.csv(file = "test.csv")

# Check out the structure of the dataset
summary(titanic)
str(titanic)
names(titanic)

# Display the first 10 rows of the data frame
head(titanic, n = 10)

# Remove rows with missing values
titanic.df <- filter(titanic, Survived != "")

ggplot(data = titanic.df) +
  aes(x = Age, fill = Survived) +
  geom_histogram(bin = 30, colour = "#1380A1") +
  #scale_fill_brewer(palette = "Accent") +
  labs(title = "Survival rate on the Titanic",
       y = "Survived",
       subtitle = "Distribution By Age, Gender and class of ticket",
       caption = "Author: etoma.egot") +
  theme_bw() + # using a custom theme for my visualizations
  #theme_bw()+ #Use the inbuilt ggplot2 them for your practice
  facet_grid(Sex~Pclass, scales = "free")
#Proportion of 1st, 2nd and  3rd class women and men who survived
mf.survived <- titanic.df %>%
  filter(Survived == 1)%>%
  group_by(Pclass,Sex)%>%
  summarise(Counts = n()
  )
mf.died <- titanic.df %>%
  filter(Survived != 1)%>%
  group_by(Pclass,Sex)%>%
  summarise(Counts = n()
  )
mf.perc.survived <- mf.survived/(mf.survived + mf.died) * 100
select (mf.perc.survived, Counts)
