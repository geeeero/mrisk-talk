# simulierte (log-)Renditewerte
library(ggplot2)

set.seed(123456)
r <- rt(510, 3, 0.3)
plot(ecdf(r))

trimsample <- function(s, ntrim) {
  s <- sort(s)
  s[-c(1:(ntrim/2), length(s)-(ntrim/2 - 1):0)]  
}
rtrim <- trimsample(r, 10)
#rtrim <- sort(r)[-c(1:5,506:510)]
plot(ecdf(rtrim))
rmean <- mean(rtrim)
rsd <- sd(rtrim)

x <- seq(-5, 5, length.out=500)
dtx <- dt(x, 3, 0.3)
plot(x, dtx, type = "l", ylim = c(0, max(dtx)))
dnx <- dnorm(x, rmean, rsd)
lines(x, dnx, col = 2)

qplot(x, dtx)

df <- data.frame(x = x, y = dtx)
#probs <- c(0, 0.05, 1)
#quantiles <- quantile(r, prob = probs)
#df$quant <- factor(findInterval(df$x, quantiles))
#ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin = 0, ymax = y, fill = quant)) +
#  scale_fill_manual(values = c("red", NA)) 

pdf("vardens1.pdf", width = 4, height = 2)
ggplot(df, aes(x, y)) + geom_line() + xlab("Renditen") + ylab("Dichte") + ylim(0, max(df$y))
dev.off()

pdf("vardens2.pdf", width = 4, height = 2)
ggplot(df, aes(x, y)) + geom_line() + xlab("Renditen") + ylab("Dichte") + ylim(0, max(df$y)) +
  geom_ribbon(data = subset(df, x < quantile(r, 0.05)), aes(ymin = 0, ymax = y), fill="red", colour = NA, alpha = 0.5)
dev.off()

#