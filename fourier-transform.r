# http://www.wolframalpha.com/share/clip?f=d41d8cd98f00b204e9800998ecf8427e444j5e41kd
# http://math.stackexchange.com/questions/633668/how-to-scale-fourier-retransformation/633745?noredirect=1#633745

a0<-1
a1<-1
a2<-1
a3<-1

maxAbsFreq <- 20
freqStepSize <- 1/100

lowerBound <- -1
upperBound <- 1

minX <- -1
maxX <- 1
stepSizeX <- 1/100

freq <- c(-(maxAbsFreq/freqStepSize):-1,1:(maxAbsFreq/freqStepSize))*freqStepSize
x <- (minX/stepSizeX):(maxX/stepSizeX)*stepSizeX

f <- function(t,a0,a1,a2,a3) a3*t^3 + a2*t^2 + a1*t + a0
I <- complex(imaginary = 1)

Fint <- function(v,t,a0,a1,a2,a3) exp(-2*I*pi*t*v) * (
	4*I*pi^3*a3*t^3*v^3 + 
	4*I*pi^3*a2*t^2*v^3 +
	6*pi^2*a3*t^2*v^2 +
	2*pi^2*a1*v^2 * (1 + 2*I*pi*t*v) +
	4*pi^2*a2*t*v^2 -
	6*I*pi*a3*t*v +
	4*I*pi^3*a0*v^3 -
	2*I*pi*a2*v -
	3*a3) / (8*pi^4*v^4)
	
F <- function(v,A,B,a0,a1,a2,a3) Fint(v,B,a0,a1,a2,a3) - Fint(v,A,a0,a1,a2,a3)
	
sol <- F(freq,lowerBound,upperBound,a0,a1,a2,a3)

f2 <- function(t) freqStepSize * sum(Mod(sol) * cos(t*freq*2*pi + Arg(sol)))
plot(x,sapply(x,f2), pch=19, col=rgb(0,0,1,alpha=.5), cex=.2)
points(x,f(x,a0,a1,a2,a3), pch=19, col=rgb(1,0,0,alpha=.5), cex=.1)
