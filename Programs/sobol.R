library(randtoolbox)
library(ggplot2)

n <- 1024
d <- 2
sbl <- sobol(n, dim = d)
rnd <- matrix(runif(n*d), ncol = d)

sobol_df <- data.frame(x = sbl[, 1], y = sbl[, 2])
random_df <- data.frame(x = rnd[, 1], y = sbl[, 2])

p <- ggplot(sobol_df, aes(x = x, y = y)) +
    geom_point() +
    theme_bw()

p1 <- ggplot(random_df, aes(x = x, y = y)) +
    geom_point() +
    theme_bw()

ggsave(p, filename = "~/Documents/MScThesis/figures/sobol.png", width = 12, height = 10, units = "in")
ggsave(p1, filename = "~/Documents/MScThesis/figures/rnd.png", width = 12, height = 10, units = "in")


