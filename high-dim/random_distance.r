max_dim <- 100
num_points <- 1000

rand_dist <- function(N, dim) {
  combinations <- expand.grid(a=1:N,b=1:N)
  combinations <- combinations[combinations$a < combinations$b,]
  
  m <- matrix(rnorm(dim*N), ncol=dim)
  
  d <- sqrt(rowSums((m[combinations$a,] - m[combinations$b,])^2))
  
  return(c(avg = mean(d), min = min(d), max = max(d)))
}

v <- mapply(rand_dist, rep(num_points,max_dim-1), 2:max_dim)

df <- data.frame(
  dim = 2:max_dim, 
  aggregation = as.factor(c(rep("min", length(v["min",])), rep("max", length(v["max",])), rep("avg", length(v["avg",])))),
  v = c(v["min",], v["max",], v["avg",])
)

ggplot(df,aes(dim,v)) + 
  geom_point(aes(colour=aggregation), size=I(3), alpha=I(0.6)) + 
  geom_line(aes(colour=aggregation), size=I(.2), alpha=I(0.5)) +
  scale_x_discrete(breaks=1:10*10) + 
  labs(title="Statistics for Distances of Standard Normally Distributed Points", 
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