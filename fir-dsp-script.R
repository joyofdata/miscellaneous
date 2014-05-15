library(signal)
library(lattice)

add_sin_sig <- function(n, freq) {
  sig <- rep(0,n)
  for(f in freq) {
    sig <- sig + sin(2*pi*1:n/floor(n/f))
  }
  return(sig)
}

sin_sig_incr_freq <- function(n, from, to) {
  sig <- sin(2*pi*1:n/floor(n/seq(from=from, to=to, length.out=n)))
}

pm_filter <- function(n_sig, n_fil, freq, type, d1=.05, d2=.07) {
  c0 <- 2 * freq / n_sig
  
  if(type == "stop") {
    v <- c(1,1,0,0,1,1)
  } else if(type == "pass") {
    v <- c(0,0,1,1,0,0)
  }
  
  fir <- remez(n = n_fil,f=c(0,c0-d2,c0-d1,c0+d1,c0+d2,1),a = v)
  freq <- freqz(fir, n=n_fil)
  return(list(fir = fir, freq = freq))
}

apply_filter_to_sig <- function(fir, sig) {
  y <- signal::filter(as.vector(fir), 1, x = sig)
}

###################

n <- 10000
F <- 500
F0 <- 2 * F / n

par(mfrow=c(1,1))

sig <- add_sin_sig(n, c(300,500,700,1100))
filter <- cheby1(6,2,c(F0-F0*.1,F0+F0*.1),type="pass")
sig2 <- signal::filter(filter, x=sig)

par(mfrow=c(1,2))
barplot(as.vector(impz(filter)$x))
plot(freqz(filter)$f,abs(freqz(filter)$h),type="l",xlim=c(0.2,0.4))

zplane(filter)

par(mfrow=c(3,2))
plot(sig[4000:5000],type="l")
plot(sig2[4000:5000],type="l")

plot(abs(fft(sig))[1:1500], type="l")
plot(abs(fft(sig2))[1:1500], type="l")

specgram(sig,n=500)
specgram(sig2,n=500)



####################

n <- 100000
n_fil <- 800
F <- 5000
F0 <- 2 * F / n

sig <- sin_sig_incr_freq(n, from = 0, to = 10000)

filter <- pm_filter(n_sig = n, n_fil = n_fil, freq = F, type = "stop", d1 = F0*.05, d2 = F0*.1)

par(mfrow=c(3,2))
a <- floor(F0 * 0.6 * n_fil)
b <- floor(F0 * 1.4 * n_fil)
plot(350:450,as.vector(filter$fir)[350:450],type="l")
plot(filter$freq$f[a:b],abs(filter$freq$h)[a:b],type="l")

G <- expand.grid(a=-100:100/100,b=-100:100/100)
for(i in 1:nrow(G)){ G[i,"v"] <- abs(sum(filter$freq$h*((G[i,"a"]+G[i,"b"]*1i)^-(0:799)))) }
levelplot(log(matrix(G[,"v"], ncol=201, byrow=TRUE))/log(10000000), col.regions=colorRampPalette(c("blue","red", "green"), space = "Lab")(100), at=(c(seq(-.65,0,length.out=50)^7,seq(0,70,length.out=51)[2:51])))

sig2 <- apply_filter_to_sig(filter$fir, sig)

par(mfrow=c(3,2))
plot(sig, type="l", col=rgb(.3,.3,.3))
plot(sig2, type="l", col=rgb(.3,.3,.3))
specgram(sig,n=50)
specgram(sig2,n=50)
plot(log(abs(fft(sig)))[1:10000], col=rgb(.3,.3,.3),type="l")
plot(log(abs(fft(sig2)))[1:10000], col=rgb(.3,.3,.3),type="l")

