# JTK Cycle V3.1 adaptation
#   Original: https://openwetware.org/wiki/File:JTKversion3.zip
#   Reference: https://journals.sagepub.com/doi/10.1177/0748730410379711

# Environment for storing JTK runtime variables.
JTK_funEnv <- new.env(parent = baseenv())
JTK_env <- new.env(parent = JTK_funEnv)

JTK_funEnv$fast.two.sum <- function(a,b) {
  # known abs(a) >= abs(b)
  x <- a+b
  bv <- x-a
  y <- b-bv
  if(y==0) return(x)
  c(x,y)
}
JTK_funEnv$two.sum <- function(a,b) {
  # unknown order
  x <- a+b
  bv <- x-a
  av <- x-bv
  br <- b-bv
  ar <- a-av
  y <- ar+br
  if(y==0) return(x)
  c(x,y)
}
JTK_funEnv$expansion.sum <- function(g) {
  g <- g[order(abs(g))]
  z <- JTK_funEnv$fast.two.sum(g[2],g[1])
  q <- z[1]
  h <- NULL
  if(length(z)!=1) h <- z[2]
  n <- length(g)
  if(n==2) return(c(h,q))
  for(i in 3:n) {
    z <- JTK_funEnv$two.sum(q,g[i])
    q <- z[1]
    if(length(z)!=1) h <- c(h,z[2])
  }
  # strongly non-overlapping values
  c(h,q)
}
JTK_funEnv$hlm <- function(z) {
  # Hodges-Lehmann estimator of the median
  zz <- outer(z,z,"+")
  zz <- zz[lower.tri(zz,diag=TRUE)]
  ###v3.1 ignore missing values
  median(zz, na.rm=TRUE)/2
}
JTK_env$JTK.AMPFACTOR <- sqrt(2) # 1/median(abs(cosine)) used to calculate amplitudes
JTK_env$JTK.PIHAT <- round(pi,4) # replacement for pi to ensure unique cos values
JTK_env$JTK.ALT <- list()			 ###v3.1 container for alternative distributions



#' JTK Cycle V3.1 Adaptation `jtkdist`
#'
#' @param timepoints Number of time points in the data.
#' @param reps Number of replicates for each time point.
#' @param normal Not used. Refer to JTK Cycle paper.
#' @param alt Not used. Refer to JTK Cycle paper.
#'
#' @returns Not used. Refer to JTK Cycle paper.
#' @export
#'
#' @examples
#' # Refer to the JTK Cycle guide.
jtkdist <- function(timepoints,reps=1,normal=FALSE,alt=FALSE) {

  if(length(reps)==timepoints) {
    tim <- reps # support for unbalanced replication
  } else {
    tim <- rep(reps[1],timepoints) # balanced replication
  }

  maxnlp <- lfactorial(sum(tim))-sum(lfactorial(tim)) #maximum possible negative log p-value
  limit <- log(.Machine$double.xmax) #largest representable nlp
  normal <- normal | (maxnlp>limit-1) #switch to normal approximation if maxnlp is too large

  if(alt) {
    lab <- paste(sort(tim), collapse=",")
    if(lab %in% names(JTK_env$JTK.ALT)) {
      alt.id <- match(lab, names(JTK_env$JTK.ALT))
      return(alt.id)
    }
    nn <- sum(tim)
    M <- (nn^2-sum(tim^2))/2
    JTK_env$JTK.ALT[[lab]] <- list()
    JTK_env$JTK.ALT[[lab]]$MAX <- M
    if(normal) {
      var <- (nn^2*(2*nn+3) -
                sum(tim^2*(2*tim+3)))/72
      JTK_env$JTK.ALT[[lab]]$SDV <- sqrt(var)
      JTK_env$JTK.ALT[[lab]]$EXV <- M/2
      return(length(JTK_env$JTK.ALT))
    }
  } else {
    JTK_env$JTK.GRP.SIZE <- tim # sizes of each replicate group
    JTK_env$JTK.NUM.GRPS <- length(tim) # timepoints = number of groups
    JTK_env$JTK.NUM.VALS <- nn <- sum(tim) # number of data values (independent of period and lag)
    JTK_env$JTK.MAX <- M <- (nn^2-sum(tim^2))/2 # maximum possible jtk statistic
    JTK_env$JTK.GRPS <- rep(1:length(tim), ti=tim)	### group labels
    JTK_env$JTK.DIMS <- c(nn*(nn-1)/2,1)

    if(normal) {
      JTK_env$JTK.VAR <-
        (nn^2*(2*nn+3) - sum(tim^2*(2*tim+3)))/72 # variance of jtk
      JTK_env$JTK.SDV <- sqrt(JTK_env$JTK.VAR) # standard deviation of jtk
      JTK_env$JTK.EXV <- JTK_env$JTK.MAX/2 # expected value of jtk
      JTK_env$JTK.EXACT <- FALSE
      return(invisible(0)) # omit calculation of exact distribution
    }
  }
  MM <- floor(M/2) ### mode of this possibly alternative jtk distribution
  cf <- as.list(rep(1,MM+1)) # initial lower half cumulative frequency distribution

  size <- tim ### sizes of each group of known replicate values
  size <- size[order(size)] # ascending order for fastest calculation
  k <- length(tim) ### number of groups of known replicate values

  N <- size[k]
  if(k>2) for(i in (k-1):2) {
    N <- c(size[i]+N[1],N)
  }
  for(i in 1:(k-1)) { # count permutations using the Harding algorithm
    m <- size[i]
    n <- N[i]

    if(n < MM) {
      P <- min(m+n,MM)
      for(t in (n+1):P) {  # zero-based offset t
        for(u in 1+MM:t) { # one-based descending index u
          cf[[u]] <- JTK_funEnv$expansion.sum( # Shewchuck algorithm
            c(cf[[u]],-cf[[u-t]]))
        }
      }
    }
    Q <- min(m,MM)
    for(s in 1:Q) { # zero-based offset s
      for(u in 1+s:MM) { # one-based ascending index u
        cf[[u]] <- JTK_funEnv$expansion.sum( # Shewchuck algorithm
          c(cf[[u]],cf[[u-s]]))
      }
    }
  }
  cf <- sapply(cf,sum)

  # cf now contains the lower-half cumulative frequency distribution;
  # append the symmetric upper-half cumulative distribution to cf

  if(M %% 2) {
    cf <- c(cf,2*cf[MM+1]-c(cf[MM:1],0))          # if M is odd (mode is duplicated)
  } else {
    cf <- c(cf,cf[MM+1]+cf[MM]-c(cf[MM:2-1],0))   # if M is even (unique mode is in lower half)
  }
  jtkcf <- rev(cf)                                # upper-tail cumulative frequencies for all integer jtk
  ajtkcf <- (jtkcf[-length(cf)]+jtkcf[-1])/2      # interpolated cumulative frequency values for all half-integer jtk

  id <- 1+0:(2*M)                           	  ### one-based indices for all jtk values
  cf <- id                                        # container for the jtk frequency distribution
  cf[!!id%%2] <- jtkcf                            # odd indices for integer jtk
  cf[!id%%2] <- ajtkcf                            # even indices for half-integer jtk
  cp <- cf/jtkcf[1]                               # all upper-tail p-values

  if(alt) {
    JTK_env$JTK.ALT[[lab]]$CP <- cp
    return(length(JTK_env$JTK.ALT))
  }
  JTK_env$JTK.CP <- cp
  JTK_env$JTK.EXACT <- TRUE
}



#' JTK Cycle V3.1 Adaptation `jtk.init`
#'
#' Initialize the JTK environment for all periods
#'
#' @param periods Periods to test on (in units of spacing `interval`).
#' @param interval Spacing of time points (in physical unit like hours).
#'
#' For example, `jtk.init(9:15, 2)` means that data time point spacing is
#' 2-hour and possible periods would be 18-hour to 30-hour (2-hour spacing).
#'
#' @returns None; Run only for side effects on `JTK_env`.
#' @export
#'
#' @examples
#' # Refer to the JTK Cycle guide.
jtk.init <- function(periods, interval=1) {

  JTK_env$JTK.INTERVAL <- interval
  JTK_env$JTK.PERIODS <- periods
  JTK_env$JTK.PERFACTOR <- rep(1:length(periods),ti=periods)

  tim <- JTK_env$JTK.GRP.SIZE
  timepoints <- JTK_env$JTK.NUM.GRPS
  timerange <- 1:timepoints-1 # zero-based time indices
  JTK_env$JTK.CGOOSV <- list()
  JTK_env$JTK.SIGNCOS <- list()

  for(i in 1:length(periods)) {
    period <- periods[i]
    time2angle <- 2*JTK_env$JTK.PIHAT/period # convert time to angle using an approximate pi value
    theta <- timerange*time2angle # zero-based angular values across time indices
    cos.v <- cos(theta) # unique cosine values at each timepoint
    cos.r <- rank(cos.v) # ranks of unique cosine values
    cos.r <- rep(cos.r,ti=tim) # replicated ranks

    cgoos <- sign(outer(cos.r,cos.r,"-"))
    cgoos <- cgoos[lower.tri(cgoos)]
    cgoosv <- array(cgoos,dim=JTK_env$JTK.DIMS)
    JTK_env$JTK.CGOOSV[[i]] <- matrix(
      ncol=period,nrow=nrow(cgoosv)
    )
    JTK_env$JTK.CGOOSV[[i]][,1] <- cgoosv

    cycles <- floor(timepoints/period) # v2.1
    range <- 1:(cycles*period)	# v2.1
    cos.s <- sign(cos.v)[range] # signs over all full cycles (v2.1)
    cos.s <- rep(cos.s,ti=tim[range])
    JTK_env$JTK.SIGNCOS[[i]] <- matrix(
      ncol=period,nrow=length(cos.s)
    )
    JTK_env$JTK.SIGNCOS[[i]][,1] <- cos.s

    for(j in 2:period) {                          # one-based half-integer lag index j
      delta.theta <- (j-1)*time2angle/2           # angles of half-integer lags
      cos.v <- cos(theta+delta.theta)             # cycle left
      cos.r <- rank(cos.v)                        # ranks of unique phase-shifted cosine values
      cos.r <- rep(cos.r,ti=tim)                  # phase-shifted replicated ranks

      cgoos <- sign(outer(cos.r,cos.r,"-"))
      cgoos <- cgoos[lower.tri(cgoos)]
      cgoosv <- array(cgoos,dim=JTK_env$JTK.DIMS)
      JTK_env$JTK.CGOOSV[[i]][,j] <- cgoosv

      cos.s <- sign(cos.v)[range]
      cos.s <- rep(cos.s,ti=tim[range])
      JTK_env$JTK.SIGNCOS[[i]][,j] <- cos.s
    }
  }
  return(invisible())
}



#' JTK Cycle V3.1 Adaptation `jtkstat`
#'
#' jtkstat: calculate the p-values for all (period,phase) combos.
#' v3.1 modified to analyze data with missing values.
#'
#' @param z Numeric vector of data at each time point.
#'
#' @returns None; Run only for side effects on `JTK_env`.
#'
#' @examples
#' # Internal use by `jtkx` only.
jtkstat <- function(z) {
  alt <- any(is.na(z)) ### flag for handling missing values
  if (alt) {
    tab <- table(JTK_env$JTK.GRPS[is.finite(z)])
    alt.id <- jtkdist(length(tab), as.integer(tab), alt = alt)
  }
  M <- switch(1 + alt, ### maximum possible S score for this distribution
              JTK_env$JTK.MAX, JTK_env$JTK.ALT[[alt.id]]$MAX)

  foosv <- sign(outer(z, z, "-"))
  foosv <- foosv[lower.tri(foosv)]
  dim(foosv) <- JTK_env$JTK.DIMS

  JTK_env$JTK.CJTK <- list()
  for (i in 1:length(JTK_env$JTK.PERIODS)) {
    JTK_env$JTK.CJTK[[i]] <- apply(JTK_env$JTK.CGOOSV[[i]], 2, function(cgoosv) {
      S <- sum(foosv * cgoosv, na.rm = TRUE) ### Kendall's S score ignoring missing values
      if (!S)
        return(c(1, 0, 0))
      jtk <- (abs(S) + M) / 2 ### two-tailed JTK statistic for this lag and distribution
      if (JTK_env$JTK.EXACT) {
        jtki <- 1 + 2 * jtk  # index into the exact upper-tail distribution
        p <- switch(1 + alt, 2 * JTK_env$JTK.CP[jtki], 2 *
                      JTK_env$JTK.ALT[[alt.id]]$CP[jtki])
      } else {
        p <- switch(
          1 + alt,
          2 * stats::pnorm(-(jtk - 1 / 2), -JTK_env$JTK.EXV, JTK_env$JTK.SDV),
          2 * stats::pnorm(
            -(jtk - 1 / 2),
            -JTK_env$JTK.ALT[[alt.id]]$EXV,
            JTK_env$JTK.ALT[[alt.id]]$SDV
          )
        )
      }
      c(p, S, S / M)	### include tau = S/M for this lag and distribution
    })
  }
  return(invisible())
}



#' JTK Cycle V3.1 Adaptation `jtkx`
#'
#' jtkx: integration of jtkstat and jtkdist for repeated use.
#'
#' @param z Numeric vector of data at each time point.
#' @param ampci Boolean, whether to compute amplitude CI and p-value.
#' @param conf Confidence level of amplitude. Only useful if `ampci == TRUE`.
#'
#' @returns None; Run only for side effects on `JTK_env`.
#' @export
#'
#' @examples
#' # Refer to the JTK Cycle guide.
jtkx <- function(z, ampci=FALSE, conf=0.8) { ###v3.1 'ampci=TRUE' for calculating amplitude confidence

  jtkstat(z) # calculate p and S for all (period,phase) combos
  pvals <- lapply(JTK_env$JTK.CJTK,function(cjtk) {
    return(cjtk[1,])
  })  # exact two-tailed p-values for all (period,phase) combos
  padj <- stats::p.adjust(unlist(pvals),"bonf") # Bonferroni adjusted two-tailed p-values
  JTK_env$JTK.ADJP <- min(padj) # global minimum adjusted p-value

  padj <- split(padj,JTK_env$JTK.PERFACTOR)
  minpadj <- sapply(padj,min) # minimum adjusted p-value for each period

  peris <- which(JTK_env$JTK.ADJP==minpadj) # indices of all optimal periods
  pers <- JTK_env$JTK.PERIODS[peris] # all optimal periods

  lagis <- lapply(padj[peris],function(z) {
    which(JTK_env$JTK.ADJP==z)
  }) # list of optimal lag indices for each optimal period
  count <- sum(sapply(lagis,length)) # total number of optimal lags for all optimal period

  bestper <- 0
  bestlag <- 0
  besttau <- 0
  maxamp <- 0
  maxamp.ci <- numeric(2)
  maxamp.pval <- 0

  for(i in 1:length(pers)) {
    per <- pers[i]
    peri <- peris[i]
    cjtk <- JTK_env$JTK.CJTK[[peri]]
    sc <- JTK_env$JTK.SIGNCOS[[peri]]
    w <- z[1:nrow(sc)]
    w <- (w-JTK_funEnv$hlm(w))*JTK_env$JTK.AMPFACTOR

    for(lagi in lagis[[i]]) {
      S <- cjtk[2,lagi] # optimal Kendall's S
      s <- sign(S)
      if(!s) s <- 1

      lag <- (per +(1-s)*per/4 -(lagi-1)/2)%%per
      signcos <- sc[,lagi]
      tmp <- s*w*signcos
      amp <- JTK_funEnv$hlm(tmp) ###v3.1 allows missing values
      if (ampci) ###v3.1 the calculation of amplitude confidence is optimal
      {
        wt <- stats::wilcox.test(tmp[is.finite(tmp)],
                          conf.int=TRUE, conf.level=conf, exact=FALSE)
        amp <- as.numeric(wt$estimate)
      }
      if(amp > maxamp) {
        maxamp <- amp
        bestper <- per
        bestlag <- lag
        besttau <- abs(cjtk[3,lagi]) ###v3.1
        if (ampci) ###v3.1
        {
          maxamp.ci <- as.numeric(wt$conf.int)
          maxamp.pval <- as.numeric(wt$p.value)
        }
      }
    }
  }
  JTK_env$JTK.PERIOD <- JTK_env$JTK.INTERVAL*bestper # period (hours) with max amp
  JTK_env$JTK.LAG <- JTK_env$JTK.INTERVAL*bestlag    # lag (hours) to peak with max amp
  JTK_env$JTK.AMP <- max(0,maxamp)                	  # max amp
  JTK_env$JTK.TAU <- besttau                         ### v3.1
  JTK_env$JTK.AMP.CI <- maxamp.ci			### confidence interval for max amp; 'JTK.AMP.CI' is 'c(0,0)' if 'ampci=FALSE'
  JTK_env$JTK.AMP.PVAL <- maxamp.pval	### p-value for max amp; 'JTK.AMP.PVAL' is '0' if 'ampci=FALSE'
}

#' JTK Cycle V3.1 Adaptation
#'
#' This is getter function to fetch computation results of `jtkx`.
#'
#' In this adaptation, `jtkx` results are stored internally in a package
#' environment `JTK_env`. This function is a getter of the enrivonment.
#'
#' @param get.AMP.CI Bool, whether to get amplitude confidence interval,
#'  which will be length=2 numeric vector.
#'
#' @returns Named numeric vector of current `jtkx` results stored in `JTK_env`.
#' If `get.AMP.CI==TRUE`, only return the amplitude confidence interval.
#' @export
#'
#'
#' @examples
#' # Refer to the JTK Cycle guide.
jtkget <- function(get.AMP.CI = FALSE){
  if (get.AMP.CI){
    return(JTK_env$JTK.AMP.CI)
  } else{
    fields <- c(
      "JTK.ADJP", "JTK.PERIOD", "JTK.LAG",
      "JTK.AMP","JTK.AMP.PVAL")
    res <- vapply(
      X = fields, FUN = \(f) eval(parse(text = f), envir = JTK_env),
      FUN.VALUE = NA_real_, USE.NAMES = TRUE
    )
    return(res)
  }
}
