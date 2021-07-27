distSeq <- seq(-10, 10, by = .1)
testPower <-function(mu, n, c) pnorm(-sqrt(c) - ((sqrt(n)*mu)/10)) + pnorm(-sqrt(c) + ((sqrt(n)*mu)/10))

png('../Desktop/Projects/Stat135/Chapter9/output/Problem11/n=25.png', type = 'cairo',
    width = 640, height = 480)
plot(distSeq, testPower(distSeq, n = 25, c = qchisq(.95, df = 1)), type = 'l',
     xlab = 'Mu', ylab = 'Power', main = 'Power of the Likelihood Test, n=25')
points(0, testPower(0, n = 25, c = qchisq(.95, df = 1)), pch = 21)
lines(distSeq, testPower(distSeq, n = 25, c = qchisq(.90, df = 1)), type = 'l', lty = 3)
points(0, testPower(0, n = 25, c = qchisq(.90, df = 1)), pch = 21)
legend(title = 'Alpha', x = 'bottomright', legend = c('.10', '.05'), lty = c(3, 1))
dev.off()

png('../Desktop/Projects/Stat135/Chapter9/output/Problem11/n=100.png', type = 'cairo',
    width = 640, height = 480)
plot(distSeq, testPower(distSeq, n = 100, c = qchisq(.95, df = 1)), type = 'l',
     xlab = 'Mu', ylab = 'Power', main = 'Power of the Likelihood Test, n=100')
points(0, testPower(0, n = 25, c = qchisq(.95, df = 1)), pch = 21)
lines(distSeq, testPower(distSeq, n = 100, c = qchisq(.90, df = 1)), type = 'l', lty = 3)
points(0, testPower(0, n = 25, c = qchisq(.90, df = 1)), pch = 21)
legend(title = 'Alpha', x = 'bottomright', legend = c('.10', '.05'), lty = c(3, 1))
dev.off()