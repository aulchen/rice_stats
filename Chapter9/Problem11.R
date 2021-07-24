distSeq <- c(seq(-10, -0.1, by = .1), seq(0.1, 10, by=.1))
n = 25
y = qchisq(.95, df = 1)
testPower <-function(mu) pnorm(-sqrt(y) + ((sqrt(n)*mu)/10)) + pnorm(-sqrt(y) - ((sqrt(n)*mu)/10))
plot(distSeq, testPower(distSeq), type = 'l')
