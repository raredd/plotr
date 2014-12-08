## pie charts in ggplot

browsers <- source(system.file('source','browsers.r', package = 'plotr'))$value

library(ggplot2)
ggplot(browsers) + 
  ## create plot in rectangular coordinates
  geom_rect(aes(fill = version, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_rect(aes(fill = browser, ymax = ymax, ymin = ymin, xmax = 3, xmin = 0)) +
  xlim(c(0, 4)) + 
  theme(aspect.ratio = 1) + 
  theme_minimal() +
  ## transform to polar coords
  coord_polar(theta = 'y')
