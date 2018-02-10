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

# Plot pie chart 2017
ggplot(person_chart_2017, aes(x = 1, y = Number, fill = Crime, 
                              label = label)) +
  geom_col(position = "fill") +
  coord_polar(theta = "y") +
  labs(title = "QLD Criminal Offences 2017", x = "", y = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", legend.text = element_text(size = 7),
        title = element_text(size = 12),
        legend.title = element_text(size = 9)) +
  geom_text(size = 4, col = "black", position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1")

getwd()

ggsave("QLD_Reported_Offences_Number_pie_chart_2017.png")