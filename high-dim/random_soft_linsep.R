library(kernlab)
library(ggplot2)
library(grid)

err_svm_for_dim <- function(num_samples, num_points_per_class, dim, C) {
  
  rand_svm <- function(N,dim, C) {
    v <- as.factor(c(rep("a",N),rep("b",N)))
    
    m <- rbind(matrix(rnorm(N*dim,-1,sqrt(dim*2^2)), ncol=dim), matrix(rnorm(N*dim,1,sqrt(dim*2^2)), ncol=dim))
    
    svm <- ksvm(m,v,kernel="vanilladot",C=C,kpar=list())
    
    return(svm@error)
  }
  
  v_res <- mapply(rand_svm, rep(num_points_per_class, num_samples), rep(dim, num_samples), rep(C, num_samples))
  
  q <- quantile(v_res, c(.1,.5,.9))
  names(q) <- NULL
  
  return(c(
    q01 = q[1], q05 = q[2], q09 = q[3], 
    ls0 = sum(v_res == 0) / num_samples, 
    ls1 = sum(v_res <= 1/(2*num_points_per_class)) / num_samples, 
    ls2 = sum(v_res <= 1/num_points_per_class) / num_samples
  ))
}

res_N10 <- sapply(1:50, function(d) err_svm_for_dim(num_samples=1000,num_points_per_class=10,dim=d,C=10))
res_N20 <- sapply(1:50, function(d) err_svm_for_dim(num_samples=1000,num_points_per_class=20,dim=d,C=10))
res_N30 <- sapply(1:50, function(d) err_svm_for_dim(num_samples=1000,num_points_per_class=30,dim=d,C=10))


df <- data.frame(
  dim = 1:50, 
  N = as.factor(c(rep(10, length(res_N10["ls0",])), rep(20, length(res_N20["ls0",])), rep(30, length(res_N30["ls0",])))),
  v = c(res_N10["ls0",],res_N20["ls0",],res_N30["ls0",])
)

ggplot(df,aes(dim,v)) + 
  geom_point(aes(colour=N), size=I(3), alpha=I(0.6)) + 
  geom_line(aes(colour=N), size=I(.2), alpha=I(0.5)) +
  scale_x_discrete(breaks=1:10*5) + 
  labs(title="Ratio of Linearly Separable Random Point Sets (N vs N)", 
       x="dimension", 
       y="ratio of linear separable point sets") + 
  scale_y_continuous(breaks=0:10/10) + 
  theme(axis.text.x = element_text(colour="black"), 
        axis.text.y = element_text(colour="black"), 
        axis.title.x = element_text(vjust=-0.5, size=15), 
        axis.title.y = element_text(vjust=-0.2, size=15), 
        plot.margin = unit(c(1,1,1,1), "cm"), 
        plot.title = element_text(vjust=2, size=17)) + 
  annotate("text", x=0, y=Inf, label="(joyofdata.de)", vjust=1.5, hjust=-.1, size=4) 