library(ggplot2)
library(grid)

max_dim <- 100

rand_dist_origin <- function(N,dim) {
  m <- matrix(rnorm(N*dim),ncol=dim)
  v <- sqrt(rowSums(m^2))
  return(c(avg = mean(v), min = min(v), max = max(v)))
}

v <- mapply(rand_dist_origin, rep(100000,99), 2:max_dim)

df <- data.frame(
  dim = 2:max_dim, 
  aggregation = as.factor(c(rep("min", length(v["min",])), rep("max", length(v["max",])), rep("avg", length(v["avg",])))),
  v = c(v["min",], v["max",], v["avg",])
)

ggplot(df,aes(dim,v)) + 
  geom_point(aes(colour=aggregation), size=I(3), alpha=I(0.6)) + 
  geom_line(aes(colour=aggregation), size=I(.2), alpha=I(0.5)) +
  scale_x_discrete(breaks=1:10*10) + 
  labs(title="Statistics for Distances of Std Norm Distributed Points from 0", 
       x="dimension", 
       y="aggregate value of distances") + 
  scale_y_continuous(breaks=0:max(v)) + 
  theme(axis.text.x = element_text(colour="black"), 
        axis.text.y = element_text(colour="black"), 
        axis.title.x = element_text(vjust=-0.5, size=15), 
        axis.title.y = element_text(vjust=-0.2, size=15), 
        plot.margin = unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(vjust=2, size=17)) + 
  annotate("text", x=0, y=Inf, label="(joyofdata.de)", vjust=1.5, hjust=-.1, size=4) 