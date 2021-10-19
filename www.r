intervalos <- seq(from = 50, to = 110, by = 5) 
data = c(88, 95, 92, 97, 96, 97, 95, 86, 91, 95, 97, 88, 85, 76, 68)
data = c(65, 67, 70, 77, 78, 78, 80, 76, 71, 75, 87, 78, 88, 90, 98)
data = c(65, 67, 72, 77, 66, 78, 62, 66, 71, 75, 68, 78, 68, 90, 98)
hist(data,  breaks = intervalos,freq = F, ylim = c(0, 0.1), xlim = c(50, 110),main='TRADICIONAL', xlab='Calificaciones', ylab = NULL, col = "aquamarine3",border = "black", labels = TRUE)
boxplot(data, horizontal=TRUE,boxwex=0.03, at=0.09,  add=TRUE, axes=TRUE,col=c("blue","green","blue"))
#medias <- c(mean(data),0.01)
lines(density(data), ylim = c(0, 1),xlim = c(3, 120), lwd = 3,col="blue")
curve(dnorm(x, mean(data), sd(data)), lwd = 3, col = "black", add = T)
legend("topleft", c("Observada", "Teórica"),lty = 1, lwd = 5, col = c("black", "blue"), bty = "n", cex = 0.8)
getmode <- function(v) {
 uniqv <- unique(v)
 uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode <- getmode(data)
#print(length(data))
#print(3.322+log(length(data)))
print(mean(data))
print(median(data))
print(mode)
print(quantile(data))
v=0.0
points(min(data),v,pch=8,col="orange")
points(max(data),v,pch=8,col="orange")
points(mode,v,pch=16,col="red")#moda
points(mean(data),v,pch=10, col='orange')#media
points(median(data),v,pch=16,col="blue")#mediana
abline(v=mean(data), col="green")
abline(v=median(data), col="blue")
abline(v=mode, col="red")