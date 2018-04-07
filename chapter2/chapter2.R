library(rethinking)
library(purrr)

## 2M1, 2M2

globe.grid <- function(w, n, prior.function) {
    p_grid <- seq( from=0 , to=1 , length.out=20 )
    likelihood <- dbinom( w , size=n , prob=p_grid )
    prior <- map_dbl(p.grid, prior)
    posterior <- normalise(prior * likelihood)

    plot( p_grid , posterior , type="b", xlab="probability of water" , ylab="posterior probability" )
    mtext(paste("total:", toString(n), "water:", toString(w)))
}


normalise <- function(p) {
    return(p / sum(p))
}


uniform.prior <- function(x) {
    return(1)
}

step.prior <- function(x) {
    if(x < 0.5)
        return(0)
    else
        return(1)
}

## globe.grid(6, 9, uniform.prior)
## globe.grid(3, 3, uniform.prior)
## globe.grid(3, 4, uniform.prior)
## globe.grid(5, 7, uniform.prior)

## globe.grid(6, 9, step.prior)
## globe.grid(3, 3, step.prior)
## globe.grid(3, 4, step.prior)
## globe.grid(5, 7, step.prior)


## 2M3
{
    earth.mars.grid <- c(0.7, 0)
    n <- 1
    w <- 0
    likelihood <- dbinom(w, n, earth.mars.grid)
    posterior <- grid.condition(uniform.prior, earth.mars.grid, likelihood)

    print(paste("earth:", posterior[1]))
    print(paste("mars:", posterior[2]))
}

{
    print.pandas <- function(grid) {
        print(paste("A:", grid[1]))
        print(paste("B:", grid[2]))
    }

    panda.grid <- c(0.1, 0.2)

    n <- 1
    ntwins <- 1
    likelihood <- dbinom(ntwins, n, panda.grid)
    posterior <- normalise(c(1, 1) * likelihood)

    ## 2H2
    print("Prob of species")
    print.pandas(posterior)

    ## 2H1
    ptwins.again <- 0.1 * posterior[1] + 0.2 * posterior[2]
    print(paste("Prob of twins again:", ptwins.again))

    ## 2H3
    singleton.birth.posterior <- normalise(posterior * dbinom(0, 1, panda.grid))
    print(paste("Prob of species after singleton birth"))
    print.pandas(singleton.birth.posterior)

    ## 2H3 alternative calculation
    print(paste("Alternative calculation"))
    print.pandas(normalise(dbinom(x=1, size=2, prob=panda.grid) * c(1, 1)))

    ## 2H4
    prior <- c(1, 1)
    genetic.test.likelihood <- c(0.8, 0.35)
    genetic.test.posterior <- normalise(prior * genetic.test.likelihood)
    print(paste("Test alone"))
    print.pandas(genetic.test.posterior)

    print("Final probability distribution")
    print.pandas(normalise(singleton.birth.posterior * genetic.test.likelihood))
}
