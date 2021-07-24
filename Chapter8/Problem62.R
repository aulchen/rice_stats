distSeq <- seq(0, 0.6, by = .01)
partA <- list()
partA$prior <- function(x) dgamma(x, shape = .25, rate = .5)
partA$posterior <- function(x) dgamma(x, shape = 20.25, rate = 102.5)
partB <- list()
partB$prior <- function(x) dgamma(x, shape = .25, rate = .025)
partB$posterior <- function(x) dgamma(x, shape = 20.25, rate = 102.025)
png('../Desktop/Projects/Stat135/Chapter8/output/Problem62Plot_a.png', type = 'cairo',
    width = 640, height = 480)
plot(distSeq, partA$prior(distSeq), type = 'l', ylim = c(0, 10),
     xlab = 'Lambda', ylab = 'Density', main = 'Gamma(.25, .5) Prior')
lines(distSeq, partA$posterior(distSeq), type = 'l', lty = 2)
legend(x = 'topright', legend = c('Prior', 'Posterior'), lty = c(1, 2))
dev.off()
png('../Desktop/Projects/Stat135/Chapter8/output/Problem62Plot_b.png', type = 'cairo',
    width = 640, height = 480)
plot(distSeq, partB$prior(distSeq), type = 'l', ylim = c(0, 10),
     xlab = 'Lambda', ylab = 'Density', main = 'Gamma(.25, .025) Prior')
lines(distSeq, partB$posterior(distSeq), type = 'l', lty = 2)
legend(x = 'topright', legend = c('Prior', 'Posterior'), lty = c(1, 2))
dev.off()
