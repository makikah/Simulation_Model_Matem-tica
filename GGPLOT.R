## DataCamp 2


library(MASS)
library(ggplot2)
library(scales)
library(plyr)
library(devtools)

## Données mammals
mammals

ggplot(mammals, aes(x = body, y = brain)) + geom_point()

ggplot(mammals, aes(x = body, y = brain)) + geom_point(alpha = 0.6) + 
  stat_smooth(method = "lm", col = "blue", se = FALSE)

ggplot(mammals, aes(x = body, y = brain)) + geom_point(alpha = 0.6) +
  coord_fixed() + scale_x_log10() + scale_y_log10() +
  stat_smooth(method = "lm", col = "red", se = FALSE, size = 1)

ggplot(mammals, aes(x = body, y = brain)) + annotation_logticks() +
  geom_point(alpha = 0.6) + 
  coord_fixed(xlim = c(10^-3, 10^4), ylim = c(10^-1, 10^4)) +
  scale_x_log10(expression("Henri weight(log"["10"]*"(Kg))"), 
                breaks = trans_breaks("log10", function(x) 10^x),
                label = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(expression("Lola weight(log"["10"]*"(g))"),
                breaks = trans_breaks("log10", function(x) 10^x),
                label = trans_format("log10", math_format(10^.x))) + 
  stat_smooth(method = "lm", col = "blue", se = FALSE, size = 1) + theme_classic()

## Données iris
iris
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_jitter(alpha = 0.6) + facet_grid(. ~ Species)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) + facet_grid(. ~ Species) +
  stat_smooth(method = "lm", se = F, col = "red")

levels(iris$Species) <- c("Setosa", "Versicolor", "Virginica")  
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species) + 
  stat_smooth(method = "lm", se = F, col = "red") +
  scale_y_continuous("Sepal Width (cm)", 
                     limits = c(2,5),
                     expand = c(0,0)) +
  scale_x_continuous("Sepal Length (cm)",
                     limits = c(4,8),
                     expand = c(0,0)) + coord_equal()

levels(iris$Species) <- c("Setosa", "Versicolor", "Virginica")
library(grid)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  coord_equal() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.margin = unit(0, "lines")) 
