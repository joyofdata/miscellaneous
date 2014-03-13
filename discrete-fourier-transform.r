I <- complex(imaginary = 1)
x <- c(1,1,0,1,1,0,1,1,0)

N <- length(x)
x.abs.max <- max(abs(max(x)), abs(min(x)))

my.fft <- function(k,x,N) x %*% exp(-I*2*pi*k*(0:(N-1))/N)
my.ifft <- function(n,X,N) (X %*% exp(I*2*pi*(0:(N-1))*n/N))/N

my.X <- sapply(0:(N-1), function(k) my.fft(k,x,N))
my.X
sapply(0:(N-1), function(n) my.ifft(n,my.X,N))

X <- fft(x)
X
fft(X, inverse=TRUE)/N

Mod(X)
Arg(X)

x.fun <- function(t) (Mod(X) %*% cos(t*(0:(N-1)) + Arg(X)))/N
x.period <- (0:(N-1))/N*2*pi
my.x <- sapply(x.period,x.fun)

plot(x.period, my.x, xlim=c(0,2*pi), ylim=c(-x.abs.max,x.abs.max))

x.fun.comp <- function(i,t) sapply(t, function(t) Mod(X)[i+1] * cos(t*i + Arg(X)[i+1]))/N

x.period.plot <- 0:999/1000*2*pi

comp <- list()
comp[[1]] <- x.fun.comp(0,x.period.plot)
comp[[2]] <- x.fun.comp(3,x.period.plot)
comp[[3]] <- x.fun.comp(6,x.period.plot)

lines(x.period.plot, comp[[1]],col="magenta")
lines(x.period.plot, comp[[2]],col="green")
lines(x.period.plot, comp[[3]],col="blue")

lines(x.period.plot, comp[[1]]+comp[[2]]+comp[[3]],col="red")
