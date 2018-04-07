library(rethinking)
library(purrr)

## 2M1, 2M2

globe.grid <- function(w, n, prior) {
    p_grid <- seq( from=0 , to=1 , length.out=20 )
    prior <- map_dbl(p_grid, prior)
    likelihood <- dbinom( w , size=n , prob=p_grid )
    unstd.posterior <- likelihood * prior
    posterior <- unstd.posterior / sum(unstd.posterior)

    plot( p_grid , posterior , type="b", xlab="probability of water" , ylab="posterior probability" )
    mtext(paste("total:", toString(n), "water:", toString(w)))

    return(posterior)
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
    prior <- c(1, 1)
    earth.mars.grid <- c(0.7, 0)
    n <- 1
    w <- 0
    likelihood <- dbinom(w, n, earth.mars.grid)
    unstd.posterior <- likelihood * prior
    posterior <- unstd.posterior / sum(unstd.posterior)

    print(paste("earth:", posterior[1]))
    print(paste("mars:", posterior[2]))
}
