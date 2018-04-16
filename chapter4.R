
library(purrr)
library(rethinking)

{
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
}

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
        data=dc
    )

    precis(fit)

    ## 10 units of weight gets you ~27 units of height (10*mean(b))

    plot(dc$weight, dc$height, col=col.alpha(rangi2, 0.5))

    post <- extract.samples(fit)
    weights <- seq(from=5, to=45, by=1)
    sim.height <- sim(fit, data=list(weight=weights), n=1e4)
    height.mean <- apply(sim.height, 2, mean)
    height.HPDI <- apply(sim.height, 2, HPDI, prob=0.89)

    lines(weights, height.mean)

    shade(height.HPDI, weights)

    ## This fit doesn't really capture the points at extreme weights A possible
    ## improvement could be to use a 2nd order polynomial.
}


{
    data(Howell1)
    d <- Howell1
    d$log.weight <- log(d$weight)

    fit <- map(
        alist(
            height ~ dnorm( mu , sigma ) ,
            mu <- a + b*log.weight ,
            a ~ dnorm( 178 , 100 ) ,
            b ~ dnorm( 0 , 100 ) ,
            sigma ~ dunif( 0 , 50 )
        ) ,
        data=d
    )

    precis(fit)

    plot(dc$weight, dc$height, col=col.alpha(rangi2, 0.5))

    post <- extract.samples(fit)
    weights <- seq(from=5, to=45, by=1)

    ## There's probably a better way to compute HPDIs for each weight, but this
    ## works...
    mu.HPDI.for.weight <- function(w) HPDI(post$a + post$b * log(w), prob=0.97)
    mu.HPDI = apply(as.array(weights), 1, mu.HPDI.for.weight)
    shade(mu.HPDI, weights)

    sim.height <- sim(fit, data=list(log.weight=log(weights)), n=1e4)
    height.mean <- apply(sim.height, 2, mean)
    height.HPDI <- apply(sim.height, 2, HPDI, prob=0.97)

    lines(weights, height.mean)

    shade(height.HPDI, weights)

    ## Much better fit. 97% of the data is inside the 97% HPDI and the line
    ## mostly follows the data (except at a couple of points at extremely low
    ## weights).
}
