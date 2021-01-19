vec <- c(-4.12, 0, 1.1, 1, 3, 4)
A <- matrix(1:4, nrow = 2, ncol = 2)
myDf <- data.frame(var1 = 1:2, var2 = 3:4)
myDf
names(myDf) <- c("X", "Y")
myDf  

myList = list(vec = vec, A = A, myDf = myDf)
str(myList)

x=rnorm(100)
y=rnx <- seq(-4, 4, l = 100)
y <- pnorm(q = x, mean = 0, sd = 2)

x <- seq(-4, 4, l
 = 100)
y <- pnorm(x = x, mean = 0, sd = 1)
plot(x, y, type = "l")


plot(x, y, type = "l", lwd = 3, main="The distribution function of a N(0, 1)")

qnorm(p = 0.90, mean = 0, sd = 2)

qf(p = 0.95, df1 = 1, df2 = 5)
x <- seq(-4, 4, l = 100)
y <- df(x, df1 = 1, df2 = 5)
plot(x, y, type = "l")

x = rpois(20, 5)
