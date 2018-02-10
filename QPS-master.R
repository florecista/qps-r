#Install packages
#install.packages('readr')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('stringr')
#install.packages('lubridate')
#install.packages('ggplot2')

# Loading packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# OFFENCES AGAINST THE PERSON

# Loading file 
data_person <- read_csv("QLD_Reported_Offences_Number.csv")

# Selecting columns of interest
person <- select(data_person, 1:25)

# Preparing dataset
person <- mutate(person, Homicide = `Homicide (Murder)` + `Other Homicide`)
person <- rename(person, Date = `Month Year`)

# Creating Year column
person$Year <- substr(person$Date, 4, 5)
person$Help <- c(rep(19, 30), rep(20, 216))
person$Year <- paste(person$Help, person$Year, sep = "")

# Transforming dataset
person <- select(person, Year, Homicide, Assault, `Sexual Offences`, 
                 Robbery, `Other Offences Against the Person`)

person_crime <- person %>%
  gather(Crime, Number, - Year) %>%
  group_by(Year, Crime) %>%
  summarise(Number = sum(Number))

person_chart <- person_crime %>%
  group_by(Year) %>%
  summarise(Total = sum(Number)) %>%
  right_join(person_crime, by = "Year") %>%
  mutate(percentage = Number/Total*100, 
         label = paste0(round(percentage), "%")) %>%
  filter(Year != 1997)

person_chart$Crime <- factor(person_chart$Crime, levels = c("Homicide", "Assault", 
                         "Sexual Offences", "Robbery",
                         "Other Offences Against the Person"), ordered = TRUE)

# Creating 2017 dataset
person_chart_2017 <- filter(person_chart, Year == "2017")

# Plot stacked bar chart
ggplot(person_chart, aes(x = Year, y = Number, 
                           fill = Crime, label = Number)) +
  geom_col() +
  coord_flip() +
  geom_text(size = 3, col = "black", position = position_stack(vjust = 0.5)) +
  labs(title = "Offences Against the Person", y = "Number") +
  theme(legend.position = "bottom", legend.text = element_text(size = 7),
        title = element_text(size = 14),
        legend.title = element_text(size = 10)) +
  scale_fill_brewer(palette = "Set1")

# Plot filled chart
ggplot(person_chart, aes(x = Year, y = Number, fill = Crime, label = label)) +
  geom_col(position = "fill") +
  coord_flip() +
  geom_text(size = 3, col = "black", position = position_fill(vjust = 0.5)) +
  labs(title = "Offences Against the Person", y = "Share") +
  theme(legend.position = "bottom", legend.text = element_text(size = 7),
        title = element_text(size = 14),
        legend.title = element_text(size = 10)) +
  scale_fill_brewer(palette = "Set1")

# Plot pie charts
ggplot(person_chart, aes(x = 1, y = Number, fill = Crime, 
                           label = label)) +
  geom_col(position = "fill") +
  coord_polar(theta = "y") +
  labs(title = "Offences Against the Person", x = "", y = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", legend.text = element_text(size = 10),
        title = element_text(size = 14)) +
  geom_text(size = 3, col = "black", position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~Year)

# Plot pie chart 2017
ggplot(person_chart_2017, aes(x = 1, y = Number, fill = Crime, 
                         label = label)) +
  geom_col(position = "fill") +
  coord_polar(theta = "y") +
  labs(title = "Offences Against the Person, 2017", x = "", y = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", legend.text = element_text(size = 7),
        title = element_text(size = 12),
        legend.title = element_text(size = 9)) +
  geom_text(size = 4, col = "black", position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1")


# OFFENDERS BY SEX

# Loading file 
data_gender <- read_csv("QLD_Reported_Offenders_Number.csv")

# Selecting columns of interest
gender_all <- select(data_gender, `Month Year`:`Other Homicide`, Assault, 
                     `Sexual Offences`, Robbery, `Other Offences Against the Person`,
                     `Unlawful Entry`, Arson, `Other Property Damage`, 
                     `Unlawful Use of Motor Vehicle`, 
                     `Other Theft (excl. Unlawful Entry)`, Fraud, 
                     `Handling Stolen Goods`, `Drug Offences`, 
                     `Prostitution Offences`, 
                     `Liquor (excl. Drunkenness)`: `Weapons Act Offences`, 
                     `Good Order Offences`, `Stock Related Offences`, 
                     `Traffic and Related Offences`, `Miscellaneous Offences`)

# Preparing dataset
gender_all <- mutate(gender_all, Homicide = `Homicide (Murder)` + `Other Homicide`)
gender_all <- rename(gender_all, Date = `Month Year`)

# Creating Year column
gender_all$Year <- substr(gender_all$Date, 4, 5)
gender_all$Help <- rep(20, 1224)
gender_all$Year <- paste(gender_all$Help, gender_all$Year, sep = "")

# Dividing dataset into "adult" and "juvenile"
gender_all <- select(gender_all, Year, Age, Sex, Homicide, Assault:`Miscellaneous Offences`)
gender_adult <- filter(gender_all, Age == "Adult")
gender_juvenile <- filter(gender_all, Age == "Juvenile")

# Adult dataset
gender_adult_sex <- gender_adult %>%
  gather(Crime, Number, Homicide:`Miscellaneous Offences`) %>%
  group_by(Year, Crime, Sex) %>%
  summarise(Number = sum(Number))

gender_adult_chart <- gender_adult_sex %>%
  group_by(Year, Crime) %>%
  summarise(Total = sum(Number)) %>%
  right_join(gender_adult_sex, by = c("Year", "Crime")) %>%
  mutate(percentage = Number/Total*100, 
         label = paste0(round(percentage), "%")) %>%
  filter(Year != "2001") %>%
  filter(Sex != "Not Stated")

# Juvenile dataset
gender_juvenile_sex <- gender_juvenile %>%
  gather(Crime, Number, Homicide:`Miscellaneous Offences`) %>%
  group_by(Year, Crime, Sex) %>%
  summarise(Number = sum(Number))

gender_juvenile_chart <- gender_juvenile_sex %>%
  group_by(Year, Crime) %>%
  summarise(Total = sum(Number)) %>%
  right_join(gender_juvenile_sex, by = c("Year", "Crime")) %>%
  mutate(percentage = Number/Total*100, 
         label = paste0(round(percentage), "%")) %>%
  filter(Year != "2001") %>%
  filter(Sex != "Not Stated")

# Plotting adult dataset
ggplot(data = gender_adult_chart, 
       aes(x = Crime, 
           y = Number, 
           fill = Sex,
           label = label)) +
  geom_col(position = "fill") + 
  coord_flip() +
  geom_text(size = 3, col = "black", position = position_fill(vjust = 0.5)) +
  labs(title = "Offenders of Crime by Sex", subtitle = "Adults", y = "Share") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 7),
        title = element_text(size = 16)) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~Year)

# Plotting juvenile dataset
ggplot(data = gender_juvenile_chart, 
       aes(x = Crime, 
           y = Number, 
           fill = Sex,
           label = label)) +
  geom_col(position = "fill") + 
  coord_flip() +
  geom_text(size = 3, col = "black", position = position_fill(vjust = 0.5)) +
  labs(title = "Offenders of Crime by Sex", subtitle = "Juveniles", y = "Share") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 7),
        title = element_text(size = 16)) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~Year)

# Plotting 2017 adult dataset
gender_adult_chart_2017 <- gender_adult_chart %>%
  filter(Year == "2017") %>%
  ungroup(Year)

ggplot(data = gender_adult_chart_2017, 
       aes(x = Crime, 
           y = Number, 
           fill = Sex,
           label = label)) +
  geom_col(position = "fill") + 
  coord_flip() +
  geom_text(size = 3, col = "black", position = position_fill(vjust = 0.5)) +
  labs(title = "Offenders of Crime by Sex, 2017", subtitle = "Adults", y = "Share") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 6),
        title = element_text(size = 14)) +
  scale_fill_brewer(palette = "Set1")

# Plotting 2017 juvenile dataset
gender_juvenile_chart_2017 <- gender_juvenile_chart %>%
  filter(Year == "2017") %>%
  ungroup(Year)

ggplot(data = gender_juvenile_chart_2017, 
       aes(x = Crime, 
           y = Number, 
           fill = Sex,
           label = label)) +
  geom_col(position = "fill") + 
  coord_flip() +
  geom_text(size = 3, col = "black", position = position_fill(vjust = 0.5)) +
  labs(title = "Offenders of Crime by Sex, 2017", subtitle = "Juveniles", y = "Share") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 6),
        title = element_text(size = 14)) +
  scale_fill_brewer(palette = "Set1")