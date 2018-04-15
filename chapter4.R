
library(purrr)
library(rethinking)

## 4M1
count <- 1e4
sample.params <- data.frame(
    mu = rnorm(count, 0, 10),
    sigma = runif(count, 0, 10)
)
y <- map2_dbl(sample.params$mu, sample.params$sigma, function(mu, sigma) rnorm(1, mu, sigma))


## 4M2

map(
    alist(
        y ~ dnorm(mu, sigma),
        mu ~ dnorm(0, 10),
        sigma ~ dunif(0, 10)
    ),
    data=c() # We don't have any data yet!
)


## 4H1
{
    data(Howell1)
    d <- Howell1
    d2 <- d[ d$age >= 18, ]

    fit <- map(
        alist(
            height ~ dnorm( mu , sigma ) ,
            mu <- a + b*weight ,
            a ~ dnorm( 156 , 100 ) ,
            b ~ dnorm( 0 , 10 ) ,
            sigma ~ dunif( 0 , 50 )
        ) ,
        data=d2
    )

    weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
    sample.heights <- link(fit, data.frame(weight=weights))
    expected.heights <- apply(sample.heights, 2, mean)
    interval <- apply(sample.heights, 2, HPDI, prob=0.89)

    ## It seems a bit wasteful to compute all these samples just to get the HPDI
    ## for these heights, but the alternative is to waste a lot of human time
    ## doing algebra...
}


## 4H2
{
    data(Howell1)
    d <- Howell1
    dc <- d[ d$age < 18, ]

    fit <- map(
        alist(
            height ~ dnorm( mu , sigma ) ,
            mu <- a + b*weight ,
            a ~ dnorm( 100 , 100 ) ,
            b ~ dnorm( 0 , 10 ) ,
            sigma ~ dunif( 0 , 50 )
        ) ,
        data=d2
    )

    ## 10 units of weight gets you 9 units of height (10*mean(b))

    plot(dc$weight, dc$height, col=col.alpha(rangi2, 0.5))

    post <- extract.samples(fit)
    weights <- seq(from=5, to=45, by=1)
    sim.height <- sim(fit, data=list(weight=weights), n=1e4)
    height.mean <- apply(sim.height, 2, mean)
    height.HPDI <- apply(sim.height, 2, HPDI, prob=0.89)

    lines(weights, height.mean)

    shade(height.HPDI, weights)

    ## This fit is complete garbage, it clearly needs to curve but it doesn't.
    ## Half of the data isn't even inside the 89% HPDI.

    ## A possible improvement would be to use a 2nd order polynomial.
}
