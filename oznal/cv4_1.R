library('ggplot2')
library('cowplot')

ggplot2::mpg

theme_set(theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

p1 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), colour="steelblue") +
  ggtitle("KPG vs. Displacement") 

p2 <- ggplot(data = mpg) + 
  geom_boxplot(outlier.colour="black", 
               outlier.shape=16, outlier.size=2, 
               notch=T, aes(y = displ)) +
  ggtitle("Displacement")

p3 <- ggplot(data = mpg) + 
  geom_boxplot(outlier.colour="steelblue", 
               outlier.shape=16, outlier.size=2, 
               notch=T, aes(y = hwy,)) +
  ggtitle("Kilometers per gallon")

plot_grid(p1, p2, p3, labels = c('A', 'B', 'C'), label_size = 12)



p1 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

p2 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

p3 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

p4 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), label_size = 12)



p1 <- ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  ggtitle("Linear regression")

p2 <- ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=FALSE),colour="red") +
  ggtitle("Polynomial regression, 2nd degree")

p3 <- ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 8, raw=FALSE),colour="orange") +
  ggtitle("Polynomial regression, 8th degree")

p4 <- ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=FALSE),colour="red") + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 8, raw=FALSE),colour="orange") +
  ggtitle("Multiple")


plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), label_size = 12)
