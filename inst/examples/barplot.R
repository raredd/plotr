## barplot with asterisks comparing groups
dat <- data.frame(Group = c("S1", "S1", "S2", "S2"),
                  Sub   = c("A", "B", "A", "B"),
                  Value = c(3,5,7,8))  

library('ggplot2')
p <- ggplot(dat, aes(Group, Value)) +
  theme_bw() + theme(panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 15)) +
  scale_fill_manual(values = c("grey80", "grey20")) +
  geom_bar(aes(fill = Sub), stat = "identity", position = "dodge", width = .5)


## to add the arcs that indicate a subgroup comparison, compute parametric 
## coordinates of a half circle and add them connected with geom_line

label.df <- data.frame(Group = c(1,1,1, 2,2,2),
                       Value = c(6.5,6.8,7.1, 9.5,9.8,10.1))

## define arc coordinates
r <- 0.15
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r*5 * sin(t)

arc.df <- data.frame(Group = x, Value = y)

p2 <- p + geom_text(data = label.df, label = "*") +
  geom_line(data = arc.df, aes(Group+1, Value+5.5), lty = 2) +
  geom_line(data = arc.df, aes(Group+2, Value+8.5), lty = 2)

## lastly, to indicate comparison between groups, build a larger circle and
## flattened it at the top
r <- .5
x <- r * cos(t)
y <- r*4 * sin(t)
y[20:162] <- y[20] ## flattens the arc

arc.df <- data.frame(Group = x, Value = y)

p2 + geom_line(data = arc.df, aes(Group+1.5, Value+11), lty = 2) +
  geom_text(x = 1.5, y = 12, label = "***")
