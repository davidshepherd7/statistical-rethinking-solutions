library(rethinking)

## 2M1

globe.grid <- function(w, n) {
    p_grid <- seq( from=0 , to=1 , length.out=20 )
    prior <- rep( 1 , 20 )
    likelihood <- dbinom( w , size=n , prob=p_grid )
    unstd.posterior <- likelihood * prior
    posterior <- unstd.posterior / sum(unstd.posterior)

    plot( p_grid , posterior , type="b", xlab="probability of water" , ylab="posterior probability" )
    mtext(paste("total:", toString(n), "water:", toString(w)))

    return(posterior)
}

## globe.grid(6, 9)
## globe.grid(3, 3)
## globe.grid(3,4)
## globe.grid(5, 7)
