library(purrr)
library(rethinking)

map <- rethinking::map

standardize <- function(x) (x - mean(x)) / sd(x)


plot.posterior.prediction.error <- function(fit, actual, n=1e4) {
    mu <- link(fit)
    mu.mean <- apply(mu, 2, mean)
    mu.PI <- apply(mu, 2, PI)

    sims <- sim(fit, n=n)
    PI <- apply(sims, 2, PI)

    plot(mu.mean ~ actual, col=rangi2)
    abline(a=0, b=1, lty=2)
    for(i in 1:length(actual)) {
        lines(rep(actual[i], 2), c(mu.PI[1, i], mu.PI[2, i]), col=rangi2)
    }
}

counterfactual <- function(fit, x, pred.data) {
    ## compute counterfactual mean (mu)
    mu <- link( fit , data=pred.data )
    mu.mean <- apply( mu , 2 , mean )
    mu.PI <- apply( mu , 2 , PI )

    ## simulate counterfactual outcomes
    sims <- sim( fit , data=pred.data , n=1e4 )
    sim.PI <- apply( sims , 2 , PI )

    lines(x, mu.mean)
    shade(mu.PI, x)
    shade(sim.PI, x)
}


{
    data(WaffleDivorce)
    d <- WaffleDivorce

    d.lds <- data.frame(
        Location=c("Utah", "Idaho", "Wyoming", "Nevada", "Arizona", "Hawaii", "Montana", "Alaska", "Washington", "Oregon", "New Mexico", "Colorado", "California", "North Dakota", "Nebraska", "Kansas", "Texas", "Oklahoma", "South Dakota", "Missouri", "Virginia", "Arkansas", "West Virginia", "Iowa", "North Carolina", "Georgia", "Maine", "South Carolina", "Kentucky", "Alabama", "Florida", "Tennessee", "Mississippi", "Vermont", "Maryland", "Indiana", "New Hampshire", "Louisiana", "Minnesota", "Delaware", "Ohio", "Wisconsin", "Illinois", "Michigan", "Connecticut", "New York", "District of Columbia", "Massachusetts", "Pennsylvania", "Rhode Island", "New Jersey"),
        percent.mormon=c(67.70, 26.42, 11.53, 6.21, 6.10, 5.17, 4.81, 4.56, 3.94, 3.76, 3.35, 2.74, 1.97, 1.49, 1.30, 1.29, 1.25, 1.21, 1.21, 1.16, 1.13, 1.03, 0.93, 0.90, 0.84, 0.82, 0.82, 0.81, 0.79, 0.77, 0.75, 0.75, 0.73, 0.73, 0.72, 0.67, 0.65, 0.64, 0.59, 0.57, 0.53, 0.46, 0.45, 0.45, 0.44, 0.41, 0.40, 0.40, 0.40, 0.39, 0.37)
    )

    d2 <- merge(d, d.lds, by="Location")

    d2$MedianAgeMarriage.s <- standardize(d2$MedianAgeMarriage)
    d2$Marriage.s <- standardize(d2$Marriage)
    d2$Divorce.s <- standardize(d2$Divorce)
    ## TODO: take logs?
    d2$percent.mormon.s <- standardize(d2$percent.mormon)

    d2$log.mormon <- log(d2$percent.mormon)
    d2$log.mormon.s <- standardize(d2$log.mormon)


    map <- rethinking::map
    fit <- map(
        alist(
            Divorce.s ~ dnorm(mu, sigma),
            mu ~ a + b.marriage.rate * Marriage.s + b.ages * MedianAgeMarriage.s + b.log.mormon * log.mormon.s,
            a ~ dnorm(0, 10),
            b.marriage.rate ~ dnorm(0, 10),
            b.ages ~ dnorm(0, 10),
            b.log.mormon ~ dnorm(0, 10),
            sigma ~ dunif(0, 10)
        ),
        data=d2
    )



    plot.posterior.prediction.error(fit, d2$Divorce.s)

    ## marriage rate counterfactual
    marriage.points <- seq(-3, 3, length.out=30)
    plot( Divorce.s ~ Marriage.s, data=d2)
    counterfactual(
        fit,
        marriage.points,
        data.frame(
            Marriage.s=marriage.points,
            ## All means are zero b/c we standardised everything
            MedianAgeMarriage.s=0,
            log.mormon.s=0
        )
    )

    ## marriage age counterfactual
    points <- seq(-3, 3, length.out=30)
    plot( Divorce.s ~ MedianAgeMarriage.s, data=d2)
    counterfactual(
        fit,
        points,
        data.frame(
            Marriage.s=0,
            MedianAgeMarriage.s=points,
            log.mormon.s=0
        )
    )


    ## mormons counterfactual
    points <- seq(-3, 3, length.out=30)
    plot( Divorce.s ~ log.mormon.s, data=d2)
    counterfactual(
        fit,
        points,
        data.frame(
            Marriage.s=0,
            MedianAgeMarriage.s=0,
            log.mormon.s=points
        )
    )

}



## Hard
{
    data(foxes)
    d <- foxes

    d$avgfood.s <- standardize(d$avgfood)
    d$area.s <- standardize(d$area)
    d$weight.s <- standardize(d$weight)
    d$groupsize.s <- standardize(d$groupsize)


    plot(d$weight.s, d$area.s)
    plot(d$weight.s, d$groupsize.s)

    weight.area.fit <- map(
        alist(
            weight.s ~ dnorm(mu, sigma),
            mu ~ a + b.area * area.s,
            sigma ~ dunif(0, 50),
            c(a, b.area) ~ dnorm(0, 10)
        ),
        data=d
    )
    precis(weight.area.fit)

    ## Basically no effect, b.area is tiny


    weight.groupsize.fit <- map(
        alist(
            weight.s ~ dnorm(mu, sigma),
            mu ~ a + b.groupsize * groupsize.s,
            sigma ~ dunif(0, 50),
            c(a, b.groupsize) ~ dnorm(0, 10)
        ),
        data=d
    )
    precis(weight.groupsize.fit)

    ## Small negative effect


    weight.group.area.fit <- map(
        alist(
            weight.s ~ dnorm(mu, sigma),
            mu ~ a + b.groupsize * groupsize.s + b.area * area.s,
            sigma ~ dunif(0, 50),
            c(a, b.groupsize, b.area) ~ dnorm(0, 10)
        ),
        data=d
    )
    precis(weight.group.area.fit)

    area.points <- c(-2, 2, out.length=20)
    plot(d$weight.s, d$area.s)
    counterfactual(
        weight.group.area.fit,
        area.points,
        data.frame(
            groupsize.s=0,
            area.s=area.points
        )
    )

    groupsize.points <- c(-2, 2, out.length=20)
    plot(d$weight.s, d$groupsize.s)
    counterfactual(
        weight.group.area.fit,
        groupsize.points,
        data.frame(
            area.s=0,
            groupsize.s=groupsize.points
        )
    )

    ## Strong effects from both, but in opposite directions! This is a case of
    ## masked relationships. Which makes sense because it's probably the area
    ## per fox that's important for how much food the fox gets, which is
    ## probably what ultimately influences weight.


    weight.all.fit <- map(
        alist(
            weight.s ~ dnorm(mu, sigma),
            mu ~ a + b.avgfood * avgfood.s + b.groupsize * groupsize.s + b.area + area.s,
            c(a, b.avgfood, b.groupsize, b.area) ~ dnorm(0, 10),
            sigma ~ dunif(0, 50)
        ),
        data=d
    )
    precis(weight.all.fit)

    ## If we fit using all three variables we have a stddev of a and b.area of
    ## 7! Clearly this is wrong. I expect that this is because avgfood and area
    ## are multicollinear and so shoudn't both be included in the same model.



    weight.food.group.fit <- map(
        alist(
            weight.s ~ dnorm(mu, sigma),
            mu ~ a + b.avgfood * avgfood.s + b.groupsize * groupsize.s,
            c(a, b.avgfood, b.groupsize) ~ dnorm(0, 10),
            sigma ~ dunif(0, 50)
        ),
        data=d
    )
    precis(weight.food.group.fit)

    avgfood.points <- c(-2, 2, out.length=20)
    plot(d$weight.s, d$avgfood.s)
    counterfactual(
        weight.food.group.fit,
        avgfood.points,
        data.frame(
            groupsize.s=0,
            avgfood.s=avgfood.points
        )
    )


    ## From the counterfactual plots it's very hard to say whether food or area
    ## is a better fit, so let's try something else. Error plots!

    walk(c(weight.food.group.fit, weight.group.area.fit), function(fit) {
        pred.weight <- link(fit)
        mean <- apply(pred.weight, 2, mean)
        mean.PI <- apply(pred.weight, 2, PI)

        sim.weight <- sim(fit, n=1e4)
        sim.weight.PI <- apply(sim.weight, 2, PI)

        dev.new()
        plot(
            mean ~ d$weight.s,
            col=rangi2,
            ylim=range(sim.weight.PI),
            xlab="Observed weights",
            ylab="Predicted weights"
        )
        abline(a=0, b=1, lty=2)

        for(i in 1:nrow(d)) {
            lines( rep(d$weight.s[i], 2), c(mean.PI[1, i], mean.PI[2, i]), col=rangi2)
        }
    })

    ## Hmm, neither of these look very good really, there are clearly other
    ## things which are important in determining the weight. Or maybe we just
    ## don't have enough data.

    ## I think the two variables are about the same in their predictive ability.
    ## All things equal I would probably use avgfood because it seems likely to
    ## be the more fundamental variable. However it's probably much harder to
    ## get data on avgfood than on the territory area, so in real life I would
    ## probably end up using area because of that.

}
