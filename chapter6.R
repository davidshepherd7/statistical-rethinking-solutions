## R code 6.31
library(purrr)
library(rethinking)
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed( 1000 )
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[ i , ]
d2 <- d[ -i , ]


m1 <- map(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ a + b1 * age,
        sigma ~ dunif(0, 50),
        a ~ dnorm(100, 100),
        b1 ~ dnorm(100, 100)
    ),
    data=d1,
    ## Often fails unless we specify some start values
    start=list(a=140, b1=20)
)


m2 <- map(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ a + b1 * age + b2 * I(age^2),
        sigma ~ dunif(0, 50),
        a ~ dnorm(100, 100),
        c(b1, b2) ~ dnorm(100, 100)
    ),
    data=d1
)

m3 <- map(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ a + b1 * age + b2 * I(age^2) + b3 * I(age^3),
        sigma ~ dunif(0, 50),
        a ~ dnorm(100, 100),
        c(b1, b2, b3) ~ dnorm(100, 100)
    ),
    data=d1
)

m4 <- map(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ a + b1 * age + b2 * I(age^2) + b3 * I(age^3) + b4 * I(age^4),
        sigma ~ dunif(0, 50),
        a ~ dnorm(100, 100),
        c(b1, b2, b3, b4) ~ dnorm(100, 100)
    ),
    data=d1
)

m5 <- map(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ a + b1 * age + b2 * I(age^2) + b3 * I(age^3) + b4 * I(age^4) + b5 * I(age^5),
        sigma ~ dunif(0, 50),
        a ~ dnorm(100, 100),
        c(b1, b2, b3, b4, b5) ~ dnorm(100, 100)
    ),
    data=d1
)

m6 <- map(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ a + b1 * age + b2 * I(age^2) + b3 * I(age^3) + b4 * I(age^4) + b5 * I(age^5) + b6 * I(age^6),
        sigma ~ dunif(0, 50),
        a ~ dnorm(100, 100),
        c(b1, b2, b3, b4, b5, b6) ~ dnorm(100, 100)
    ),
    data=d1
)



## 6H1
compare(m1, m2, m3, m4, m5, m6)

## m4, m5 and m6 have similar WAIC, the other three are much much larger (i.e.
## terrible models). The weight suggests that m4 is about twice as likely to
## give the best answer of the models on a prediction as m5 or m6 individually.
## None of them are standout winners so we should probably use model averaging
## to combine the top three.

## This implies that we need a 4th order polynomial to get anything like the
## data.


posterior.predictive.plot <- function (fit) {
    age.seq <- seq(-3, 4, length.out = 30)

    mu <- link(fit, data=data.frame(age=age.seq))
    mu.means <- apply(mu, 2, mean)
    mu.HPDI <- apply(mu, 2, HPDI)

    sim.height <- sim(fit, data=list(age=age.seq))
    sim.height.PI <- apply(sim.height, 2, PI, prob=0.97)

    plot(d1$age, d1$height, col=col.alpha(rangi2, 0.5))
    lines(age.seq, mu.means)

    shade(mu.HPDI, age.seq)
    shade(sim.height.PI, age.seq)

    title("Model distribution for individual fit")
}


posterior.predictive.plot(m4)


## m1 and m2 are obviously terrible fits, they don't reproduce important
## features of the data. m3 is a bit more reasonable on most of the dataset, but
## predicts heights shooting up again in extreme old age which seems very
## unlikely.

## m4, m5, and m6 are all very similar and appear to fit the data well. This
## makes sense because their lower order parameters (a, b1, b2) are similar and
## the highest parameters (b5, b6) are smaller where they appear.
purrr::map(c(m4, m5, m6), precis)


dev.new()
{
    age.seq <- seq(-3, 4, length.out = 30)
    ensemble.fit <- ensemble(m1, m2, m3, m4, m5, m6, data=data.frame(age=age.seq))

    mu <- ensemble.fit$link
    mu.means <- apply(mu, 2, mean)
    mu.HPDI <- apply(mu, 2, HPDI)

    sim.height <- ensemble.fit$sim
    sim.height.PI <- apply(sim.height, 2, PI, prob=0.97)

    plot(d1$age, d1$height, col=col.alpha(rangi2, 0.5))
    lines(age.seq, mu.means)

    shade(mu.HPDI, age.seq)
    shade(sim.height.PI, age.seq)

    title("Model averaged distribution")
}

## The shape is almost identical to the individual models, but it is a little
## bit more uncertain about heights at the highest and lowest ages.



deviance <- function(fit, mu.function, data) {
    ages <- data$age
    heights <- data$height

    mu <- mu.function(ages)
    sigma <- coef(fit)["sigma"]

    log.likelihoods = dnorm(heights, mu, sigma, log=TRUE)
    return(-2 * sum(log.likelihoods))
}

mus <- c(
    function(age) coef(m1)["a"] + coef(m1)["b1"] * age,
    function(age) coef(m2)["a"] + coef(m2)["b1"] * age + coef(m2)["b2"] * age^2,
    function(age) coef(m3)["a"] + coef(m3)["b1"] * age + coef(m3)["b2"] * age^2 + coef(m3)["b3"] * age^3,
    function(age) coef(m4)["a"] + coef(m4)["b1"] * age + coef(m4)["b2"] * age^2 + coef(m4)["b3"] * age^3 + coef(m4)["b4"] * age^4,
    function(age) coef(m5)["a"] + coef(m5)["b1"] * age + coef(m5)["b2"] * age^2 + coef(m5)["b3"] * age^3 + coef(m5)["b4"] * age^4 + coef(m5)["b5"] * age^5,
    function(age) coef(m6)["a"] + coef(m6)["b1"] * age + coef(m6)["b2"] * age^2 + coef(m6)["b3"] * age^3 + coef(m6)["b4"] * age^4 + coef(m6)["b5"] * age^5 + coef(m6)["b6"] * age^6
)


in.sample.deviance <- purrr::map2_dbl(c(m1, m2, m3, m4, m5, m6), mus, function(fit, mu) deviance(fit, mu, d1))
out.of.sample.deviance <- purrr::map2_dbl(c(m1, m2, m3, m4, m5, m6), mus, function(fit, mu) deviance(fit, mu, d2))

out.of.sample.deviance.from.min <- purrr::map_dbl(out.of.sample.deviance, function(d) d - min(out.of.sample.deviance))

out.of.sample.deviance.from.min
compare(m1, m2, m3, m4, m5, m6)

## The WAIC estimates are fairly accurate: they get the order right and they are
## all the right order of magnitude. They are mostly within one SE of the true
## deviances.




m.tight.priors <- map(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ a + b1 * age + b2 * I(age^2) + b3 * I(age^3) + b4 * I(age^4) + b5 * I(age^5) + b6 * I(age^6),
        sigma ~ dunif(0, 50),
        a ~ dnorm(100, 100),
        c(b1, b2, b3, b4, b5, b6) ~ dnorm(0, 5)
    ),
    data=d1
)

precis(m.tight.priors)
deviance(m.tight.priors, mu.function=mus[[6]], data=d2) - min(out.of.sample.deviance)

posterior.predictive.plot(m.tight.priors)

## It's basically the same as m6, so I guess we have enough data to constrain
## the fit without needing regularizing priors. Or possibly the other priors are
## already regularizing enough?
