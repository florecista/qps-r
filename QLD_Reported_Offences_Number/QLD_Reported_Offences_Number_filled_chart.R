# Loading packages
library(ggplot2)

source("QLD_Reported_Offences_Number_common.R")

# Creating 2017 dataset
person_chart_2017 <- filter(person_chart, Year == "2017")

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

ggsave("QLD_Reported_Offences_Number_filled_chart.png")