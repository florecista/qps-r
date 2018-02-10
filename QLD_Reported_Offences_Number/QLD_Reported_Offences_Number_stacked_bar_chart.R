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

ggsave("QLD_Reported_Offences_Number_stacked_bar_chart.png")