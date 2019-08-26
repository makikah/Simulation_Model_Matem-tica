## Plot Time Series Data Using GGPlot

# Basic ggplot of time series

library(ggplot2)
theme_set(theme_minimal())

# demo dataset
head(economics)

# Basic line plot
par(mfrow = c(2,1))
ggplot(data = economics, aes(x = date, y = pop))+
  geom_line(color = "#00AFBB", size = 2)

ss = subset(economics, date > as.Date("2006-1-1"))

ggplot(data = ss, aes(x = date, y = pop)) + 
  geom_line(color = "#FC4E07", size = 2)

# Contrôler la taille de la ligne par la valeur d'une variable continue:
ggplot(data = economics, aes(x = date, y = pop)) +
  geom_line(aes(size = unemploy/pop), color = "#FC4E07")

## Plot multiple time series data
library(dplyr)
library(tidyr)

df = economics %>% select(date, psavert, uempmed) %>% gather(key = "variable",
                                                             value = "value", -date)
head(df)

# Multiple line plot
ggplot(df, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

# Area plot
ggplot(df, aes(x = date, y = value)) + 
  geom_area(aes(color = variable, fill = variable), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

# Définir les limites de l’axe des dates

# Base plot with date axis
henri = ggplot(data = economics, aes(x = date, y = psavert)) + 
  geom_line(color = "#00AFBB", size = 1)
henri

# Set axis limits c(min, max)
min <- as.Date("2002-1-1")
max <- NA
henri + scale_x_date(limits = c(min, max))

# Format mois/années
henri + scale_x_date(date_labels = "%b/%Y")

# Add trend smoothed line (Ajouter une ligne lissée de tendance)
henri + stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess")

## ggplot extensions for ts objects
install.packages(c("ggfortify", "changepoint", "strucchange", "ggpmisc"))

# Après installation des packages :

library(ggfortify)
library(magrittr) # for piping %>%

# Plot ts objects
autoplot(AirPassengers)

# Identify change points in mean and variance
AirPassengers %>%
  changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot()

# Detect jump in a data
strucchange::breakpoints(Nile ~ 1) %>%
  autoplot()

# Detect peaks and valleys
library(ggpmisc)

ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")+
  ylim(-500, 7300)
#############################################################

## Data visualization with ggplot2

library(hexbin)
library(ggplot2)  

surveys_complete <- Anscombe_Data

ggplot(data = surveys_complete)

ggplot(data = surveys_complete, mapping = aes(x = Y1, y = Y2)) + geom_point()

henri = TSdados %>% select(YEAR, LP, IK, PTF, Lhab) %>% gather(key = "variable",
                                                             value = "value", -YEAR)
series = data.frame(TSdados)

ggplot(data = series[1:124], mapping = aes(x = YEAR, 
                                     y = n, 
                                     color = NULL)) +
  geom_line()

library(scales)
ggplot(henri, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
library(rlang)

hen = TSdados %<% count(YEAR, Lhab)
ggplot(data = hen, mapping = aes(x = YEAR, y = n)) + geom_line()
