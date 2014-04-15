library(ggplot2)
library(grid)

vol_of_sphere <- function(dim, N) {
  m <- matrix(runif(dim*N,-1,1), ncol=dim)
  sum(sqrt(rowSums(m^2)) <= 1) / N
}

df <- data.frame(
  dim <- 1:10,
  v <- sapply(1:10, function(dim) vol_of_sphere(dim,1000000))
)

ggplot(df,aes(dim,v)) + 
  geom_point(colour=I("red"), size=I(5), alpha=I(0.5)) + 
  geom_line(colour=I("red"), size=I(.2), alpha=I(0.5)) +
  scale_x_discrete() + 
  labs(title="Ratio of Volume of Sphere in n Dimensions to Containing Box", 
       x="dimension", 
       y="ratio of sphere to containing box") + 
  scale_y_continuous(breaks=0:10/10) + 
  theme(axis.text.x = element_text(colour="black"), 
        axis.text.y = element_text(colour="black"), 
        axis.title.x = element_text(vjust=-0.5, size=15), 
        axis.title.y = element_text(vjust=-0.2, size=15), 
        plot.margin = unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(vjust=2, size=17)) + 
  annotate("text", x=Inf, y=Inf, label="(joyofdata.de)", vjust=2, hjust=1.1, size=4) 