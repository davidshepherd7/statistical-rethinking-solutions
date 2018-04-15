library(rethinking)
library(purrr)


## Easy
{

    p.grid <- seq( from=0 , to=1 , length.out=1000 )
    prior <- rep( 1 , 1000 )
    likelihood <- dbinom( 6 , size=9 , prob=p.grid )
    posterior <- likelihood * prior
    posterior <- posterior / sum(posterior)

    set.seed(100)
    count = 1e4
    samples = sample( p.grid , prob=posterior , size=count , replace=TRUE )

    ## prob of water fraction < 0.2
    sum(samples < 0.2 ) / count

    ## prob of water fraction > 0.8
    sum(samples > 0.8 ) / count

    ## prob between the two
    sum((samples < 0.8) & (samples > 0.2)) / 1e4

    ## hist(samples)

    ## value of p such that 20% of prob. mass is below it
    quantile(samples, 0.2)

    ## value of p such that 20% of prob. mass is above it
    quantile(samples, 0.8)

    ## Highest density range
    HPDI(samples, prob=0.66)

    ## Middle 66% of posterior prob
    quantile(samples, 0.166666)
    quantile(samples, 1 - 0.166666)

}


## Medium: no priors
{
    print("15 spins, 8 water, no priors")

    p.grid <- seq( from=0 , to=1 , length.out=1000 )
    prior <- rep( 1 , 1000 )
    likelihood <- dbinom( 8, size=15 , prob=p.grid )
    posterior <- likelihood * prior
    posterior <- posterior / sum(posterior)

    set.seed(100)
    count = 1e4
    samples = sample( p.grid , prob=posterior , size=count , replace=TRUE )

    print("HPDI")
    print(HPDI(samples, prob=0.9))


    ## 2M3
    nwater.samples = map_dbl(samples, function(s) rbinom(1, size=15, prob = s))
    ## nwater.samples2 = rbinom(1e4, size=15, prob = samples)
    ## hist(nwater.samples)

    print("Probability of getting 8/15 according to samples")
    print(sum(nwater.samples == 8) / 1e4)
    table(nwater.samples) / 1e4

    print("Prob of 6/9")
    nwater.samples.9.throws = map_dbl(samples, function(s) rbinom(1, size=9, prob=s))
    print(sum(nwater.samples.9.throws == 6) / count)

    dev.new()
    hist(nwater.samples)
}


## Medium, with priors
{
    print("15 spins, 8 water, with prior knowledge that 50% is water")

    p.grid <- seq( from=0 , to=1 , length.out=1000 )
    prior <- map_dbl(p.grid, function(p) if(p > 0.5) 1 else 0)
    likelihood <- dbinom( 8, size=15 , prob=p.grid )
    posterior <- likelihood * prior
    posterior <- posterior / sum(posterior)

    set.seed(100)
    count = 1e4
    samples = sample( p.grid , prob=posterior , size=count , replace=TRUE )

    print("HPDI")
    print(HPDI(samples, prob=0.9))


    ## 2M3
    nwater.samples = map_dbl(samples, function(s) rbinom(1, size=15, prob = s))
    ## nwater.samples2 = rbinom(1e4, size=15, prob = samples)
    ## hist(nwater.samples)

    print("Probability of getting 8/15 according to samples")
    print(sum(nwater.samples == 8) / 1e4)
    table(nwater.samples) / 1e4

    print("Prob of 6/9")
    nwater.samples.9.throws = map_dbl(samples, function(s) rbinom(1, size=9, prob=s))
    print(sum(nwater.samples.9.throws == 6) / count)

    dev.new()
    hist(nwater.samples)
}


normalise <- function(x) {
    return (x / sum(x))
}



## Hard
{
    data(homeworkch3)

    n <- length(birth1) + length(birth2)
    n.boys <- sum(birth1) + sum(birth2)

    p.grid <- seq(0, 1, length.out=1000)
    prior <- map_dbl(p.grid, function(p) 1)
    likelihood <- dbinom(n.boys, n, prob=p.grid)
    posterior <- normalise(prior * likelihood)

    plot(p.grid, posterior)
    print(paste( "Posterior max is at p =", p.grid[ which.max(posterior) ]))


    sample.count = 1e4
    p.samples = sample(p.grid, prob=posterior, size=sample.count, replace=TRUE)
    hist(p.samples)

    HPDI(p.samples, prob=c(0.5, 0.89, 0.97))

    b.samples = map_dbl(p.samples, function(p) rbinom(n=1, size=200, prob=p))
    dens(b.samples)
    abline(v=n.boys)

    ## Yes, it fits well because the real data is right in the middle of a
    ## Gaussian peak.


    first.born.samples = map_dbl(
        sample(p.grid, prob=posterior, size=sample.count, replace=TRUE),
        function(p) rbinom(n=1, size=100, prob=p)
    )
    dens(first.born.samples, norm.comp=TRUE, show.HPDI=0.50)
    abline(v=sum(birth1))
    abline(v=sum(birth2))

    ## Not so good now, it's on the edge of the Gaussian.

    second.after.girls = birth2[birth1 == 0]
    sum(second.after.girls)

    after.girls.samples = map_dbl(
        sample(p.grid, prob=posterior, size=sample.count, replace=TRUE),
        function(p) rbinom(n=1, size=length(second.after.girls), prob=p)
    )
    hist(after.girls.samples)
    abline(v=sum(second.after.girls))

    ## No, the model is useless at predicting this. Clearly the two births are
    ## related and after a girl a boy is more likely to be born.
}
