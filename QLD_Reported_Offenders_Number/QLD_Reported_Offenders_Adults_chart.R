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