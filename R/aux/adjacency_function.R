adj <- read.csv("manual/data/adjacency.csv")


madj <- lm(seed_limit ~ adj, data = adj)

ggscatter(adj, x= "adj", y = "seed_limit",
          add = "reg.line",
          add.params = list(color = "black", fill = "lightgray"),
          conf.int = TRUE,
          xlab = "% Adjacency",
          ylab = "Cumulative seed limitation"
          ) + ylim(0,1) +
  stat_cor(label.x = 75, label.y = 0.9) +
  stat_regline_equation(label.x = 75, label.y =1)





# La seed limitation se rige por la siguient ecuación 0.7330 - 0.0039*adj
a <- 0.7330
b <- -0.0039

# Entonces, a mayor adjacencia, menor seed limitation

nd <- data.frame(
  adj = seq(0, 100, 1))

nd <- nd %>%
  mutate(seedlimit = a+b*nd$adj,
         seed_entry = 1 - seedlimit,
         seed_entrystd = (seed_entry - .267)/(.657 - 0.267))

plot(nd$seedlimit ~ nd$adj, type ="l", ylim=c(0,1), xlab="Adjacency (%)")
lines(nd$seed_entry ~ nd$adj, add=TRUE, col="blue")
lines(nd$seed_entrystd ~ nd$adj, add=TRUE, col="red")



0% 1 - 0.7330
100 % 1 - 0.3430


nd$seed_limit <- a+b*nd$adj
nd$inv_seed_limit <- 1/nd$seed_limit






adjF <- ((seed_limitation_i - sl0) / (sl100 - sl0)) + 0.5


