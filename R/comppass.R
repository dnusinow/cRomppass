#' @importFrom dplyr bind_cols
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%

## library("dplyr")
## library("magrittr")

## Calculates Shannon entropy for a list of values. Because the log of zero is
## undefined, a fractional pseudocount is added to each value. This pseudocount
## is set to 1/ # of values.  
##
## @param xs A vector of values to calculate the entropy for
## @return The calculated entropy value
entropy <- function(xs) {
    p <- (xs + 1/length(xs)) / (sum(xs) + 1)
    ent <- sum(sapply(p, function(x) { -1*x*log(x, 2) }))
    return(ent)
}

## Normalizes a vector of WD scores for a given normalization factor
##
## @param xs A vector of unnormalized WD scores
## @param norm.factor A number between 0 and 1 corresponding to the quantile of the xs to normalize to. Defaults in the comppass function to the 98th percentile.
## @return A vector of WD scores divided by whatever score is the percentile specified by norm.factor
normalize.wd <- function(xs, norm.factor) {
    xs.f <- Filter(function(x) { ! (is.nan(x) || is.na(x)) }, xs)
    return(xs / quantile(xs.f, norm.factor)[1])
}

calc.z.and.wd <- function(row.df, stats, n.experiments) {
    ## This only includes experiments besides the current one
    ## where that prey was seen
    cur.experiment <- row.df[1, "Experiment.ID"]
    cur.prey <- row.df[1, "Prey"]
    all.other.prey.spectral.counts.df <- stats[stats$Bait != cur.prey &
                                               stats$Prey == cur.prey,]
    
    if(nrow(all.other.prey.spectral.counts.df) == 0) {
        ## The prey wasn't in the stats table so just use the given experiment
        all.other.prey.spectral.counts.df <- row.df
        n.experiments.with.prey <- 1
        n.experiments.without.prey <- n.experiments
    } else {
        n.experiments.with.prey <- unique(all.other.prey.spectral.counts.df$N.Experiments.With.Prey)
        n.experiments.without.prey <- unique(all.other.prey.spectral.counts.df$N.Experiments.Without.Prey)
    }


    with(all.other.prey.spectral.counts.df, {
        ## The prey mean has to include the 0 spectral counts for each
        ## bait where that prey wasn't seen, so we divide by the total
        ## number of experiments rather than the number of experiments
        ## that saw that prey
        prey.mean <- sum(AvePSM) / n.experiments

        ## Calculate the SD. The sum of squared error has those preys
        ## that were seen and those than weren't and thus have a
        ## deviation from the mean of the mean.
        ## n.experiments.with.prey <- length(all.other.prey.spectral.counts.df$AvePSM) + 1
        ## n.experiments.without.prey <- n.experiments - n.experiments.with.prey
        prey.sum.squared.err <- sum(raise_to_power(AvePSM - prey.mean, 2)) +
            (raise_to_power(prey.mean, 2) * n.experiments.without.prey)
        prey.sd <- sqrt(prey.sum.squared.err / (n.experiments - 1))

        z <- (row.df$AvePSM - prey.mean) / prey.sd
        
        wd.inner.term <- (n.experiments / n.experiments.with.prey) *
            (prey.sd / prey.mean)
        wd <- sqrt(row.df$AvePSM * raise_to_power(wd.inner.term, row.df$N.Saw))

        return(c("SumAPSM" = sum(AvePSM), "Mean" = prey.mean, "SD" = prey.sd, "Little.N" = n.experiments.with.prey, "Little.P" = row.df$N.Saw, "Z" = z, "WD" = wd))
    })
}

#' Calculates Z-Scores, WD scores, and entropy for a given input
#' table of APMS runs
#'
#' @param input A table of experiments, replicates, baits, prey, and spectral counts. See details.
#' @param stats An optional externally calculated stats table. Must be in the same form as the input table. Defaults to NULL, in which case the stats are calculated from the input table.
#' @param norm.factor factor A number between 0 and 1 corresponding to the quantile of the xs to normalize to. Defaults to the 98th percentile.
#' @return A data frame containing the following columns: Experiment.ID, Bait, Prey, AvePSM, Z, WD, Entropy
#' @examples
#' data(comppass_test_data)
#' comppass.test.out <- comppass(comppass.test.data)
#' @export
comppass <-function(input, stats = NULL, norm.factor = 0.98) {
    
    ## Calculate the average spectral count for each replicate and the
    ## entropy while we're at it
    ave.psm <- input %>%
        group_by(Experiment.ID, Prey, Replicate) %>%
        summarize(Bait = unique(Bait),
                  Spectral.Count = max(Spectral.Count)) %>%
        ungroup() %>%
        group_by(Experiment.ID, Prey) %>%
        summarize(Bait = unique(Bait),
                  AvePSM = mean(Spectral.Count),
                  N.Saw = length(which(Spectral.Count > 0)),
                  Entropy = entropy(Spectral.Count)) %>%
        ungroup() %>%
        as.data.frame()

    ## Set up the stats table. If it's implicit just assign the
    ## calculated input file. If it's explicit we need to calculate
    ## the same average
    if(is.null(stats)) {
        stats <- ave.psm
        norm.value <- NULL
    } else {
        stats %<>%
            group_by(Experiment.ID, Prey) %>%
            summarize(Bait = unique(Bait),
                      AvePSM = mean(Spectral.Count),
                      N.Saw = length(which(Spectral.Count > 0))) %>%
            ungroup()

        stats.comppass <- comppass(stats, norm.factor = NULL)
        norm.value <- quantile(stats.comppass$WD, norm.factor)[1]
    }

    n.experiments <- length(unique(stats$Experiment.ID))
    stats %<>%
        group_by(Prey) %>%
        mutate(N.Experiments.With.Prey = length(unique(Experiment.ID)),
               N.Experiments.Without.Prey = n.experiments - N.Experiments.With.Prey) %>%
        ungroup()

    all.prey.names <- unique(ave.psm$Prey)
    prey.idx.map <- lapply(all.prey.names, function(prey) {
        which(stats$Prey == prey)
    })
    names(prey.idx.map) <- all.prey.names
    
    scores.l <- lapply(seq(nrow(ave.psm)), function(i) {
        cur.prey <- ave.psm[i, "Prey"]
        cur.prey.idx <- prey.idx.map[[cur.prey]]
        if(is.null(cur.prey.idx)) {
            sub.stats <- ave.psm[i,]
        } else {
            sub.stats <- stats[cur.prey.idx,]
        }
        calc.z.and.wd(ave.psm[i,], sub.stats, n.experiments)
    })

    scores <- scores.l %>%
        do.call("rbind", .) %>%
        as.data.frame() %>%
        bind_cols(ave.psm, .) %>%
        select(Experiment.ID, Bait, Prey, AvePSM, SumAPSM, Mean, SD, Little.N, Little.P, Z, WD, Entropy)

    if(is.null(norm.value) & (! is.null(norm.factor))) {
        scores$WD <- normalize.wd(scores$WD, norm.factor)
    } else {
        if( ! is.null(norm.value) ) {
            scores$WD <- scores$WD / norm.value
        }
    }
    
    return(scores)
}
