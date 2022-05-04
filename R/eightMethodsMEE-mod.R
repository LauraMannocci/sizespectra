##' Use all eight methods to fit a simple vector of body masses as in the MEE paper
##'
##' Use all eight of the methods described in MEE paper to fit size spectra to a
##' vector of body masses. Data could be lengths but then the LBmiz, LBbiom and
##' LBNbiom methods are meaningless.
##'
##' @param x vector of individual body masses
##' @param num.bins suggested number of bins for Llin, LT and LTplus1 methods
##' @param b.only TRUE only returns slope or b plus confidence intervals, else
##'   return full details
##' @return If `b.only` is TRUE then return a list with each element (one for
##'   each method) being a
##'   vector the slope or b followed by its confidence interval.
##'   If `b.only` is FALSE then return list of full results (need to specify
##'   here -- see example in vignette `MEE_reproduce_1`).

##' @export
##' @import sizeSpectra
##' @author Andrew Edwards
eightMethodsMEE_mod <- function(x,
                            num.bins = 8,
                            b.only = FALSE){
  xmin = min(x)    # To avoid keep calculating
  xmax = max(x)
  log.x = log(x)
  sum.log.x = sum( log.x )
  
  # Notation:
  # hAAA - h(istrogram) for method AAA.
  
  # Llin method
  hLlin.list = Llin.method(x, num.bins = num.bins)
  
  # LT method - plotting binned data on log-log axes then fitting regression,
  #  as done by Boldt et al. 2005, natural log of counts plotted against natural
  #  log of size-class midpoints.
  
  # Use Llin method's binning.
  hLT.log.mids = log(hLlin.list$mids)
  hLT.log.counts = log(hLlin.list$counts)
  hLT.log.counts[ is.infinite(hLT.log.counts) ] = NA
  # lm can't cope with -Inf, which appear if 0 counts in a bin
  
  hLT.lm = lm(hLT.log.counts ~ hLT.log.mids, na.action=na.omit)
  
  hLT.list = list(log.mids = hLT.log.mids,
                  log.counts = hLT.log.counts,
                  lm = hLT.lm,
                  slope = hLT.lm$coeff[2],
                  confVals = confint(hLT.lm, "hLT.log.mids", 0.95))
  # breaks = hLlin$breaks,
  # LTplus1 method - plotting linearly binned data on log-log axes then fitting
  #  regression of log10(counts+1) vs log10(midpoint of bins), as done by
  #  Dulvy et al. (2004).
  
  # Use Llin method's binning.
  hLTplus1.log10.mids = log10(hLlin.list$mids)
  hLTplus1.log10.counts = log10(hLlin.list$counts + 1)
  hLTplus1.log10.counts[ is.infinite(hLTplus1.log10.counts) ] = NA
  # lm can't cope with -Inf, which appear if 0 counts in a bin
  #  though the + 1 avoids this issue here
  hLTplus1.lm = lm( hLTplus1.log10.counts ~ hLTplus1.log10.mids, na.action=na.omit)
  
  hLTplus1.list = list(log10.mids = hLTplus1.log10.mids,
                       log10.counts = hLTplus1.log10.counts,
                       lm = hLTplus1.lm,
                       slope = hLTplus1.lm$coeff[2],
                       confVals = confint(hLTplus1.lm, "hLTplus1.log10.mids", 0.95))
  # breaks = hLlin$breaks,
  
  # LBmiz method - binning data using log10 bins, plotting results on natural
  #  log axes (as in mizer). Mizer does abundance size spectrum or biomass
  #  size spectrum - the latter multiplies abundance by the min of each bin
  #  (see below).
  
  # Construction of bins is as follows, from Finlay Scott:
  # The bins dimensions can be specified by the user by passing in min_w, max_w
  #  (min values for the lowest and highest bins) and no_w (number of bins)
  #  arguments. These are then used:
  #    w <- 10^(seq(from=log10(min_w), to=log10(max_w), length.out=no_w))
  #    dw <- diff(w)
  #    dw[no_w] <- dw[no_w-1] # Set final dw as same as penultimate bin
  #
  # The w values are the break points of the bins (the start of the bin).
  
  hLBmiz.num.bins = num.bins
  
  beta = nlm(LBmizbinsFun, 2, xmin=xmin, xmax=xmax, k=hLBmiz.num.bins)$est
  
  hLBmiz.bins = c(beta^(0:(hLBmiz.num.bins-1)) * min(x), max(x))
  # Mizer bin specification, with final bin being same width as penultimate bin
  
  hLBmiz = hist(x, breaks=hLBmiz.bins, plot=FALSE)     # linear scale
  
  # From mizer's getCommunitySlopeCode.r:
  #  "Calculates the slope of the community abundance through time by
  #  performing a linear regression on the logged total numerical abundance
  #  at weight and logged weights (natural logs, not log to base 10, are used)."
  #  So regress log(total counts) against log(weights) (not log10 and not
  #  normalised). And it's actually on the minima of the bins (their w).
  
  hLBmiz.log.min.of.bins = log(hLBmiz.bins[-length(hLBmiz.bins)]) # min of each bin
  hLBmiz.log.counts = log(hLBmiz$counts)
  hLBmiz.log.counts[ is.infinite(hLBmiz.log.counts) ] = NA
  # lm can't cope with -Inf, which appear if 0 counts in a bin
  
  hLBmiz.lm = lm( hLBmiz.log.counts ~ hLBmiz.log.min.of.bins, na.action=na.omit)
  
  hLBmiz.list = list(log.min.of.bins = hLBmiz.log.min.of.bins,
                     log.counts = hLBmiz.log.counts,
                     lm = hLBmiz.lm,
                     slope = hLBmiz.lm$coeff[2],
                     confVals = confint(hLBmiz.lm, "hLBmiz.log.min.of.bins", 0.95))
  # breaks = hLlin$breaks,
  
  # LBbiom method - binning data using log2 bins, calculating biomass (not counts)
  #  in each bin, plotting log10(biomass in bin) vs log10(midpoint of bin)
  #  as done by Jennings et al. (2007), who used bins defined by a log2 scale.
  
  hLBNbiom.list = LBNbiom.method(x)    # Does this LBbiom and LBNbiom methods.
  
  # LBNbiom method - on biomass, not counts, as per Julia Blanchard's 2005 paper.
  #  log2 bins of bodymass, sum the total biomass in each bin, normalise
  #  biomasses by binwidths, fit regression to log10(normalised biomass) v
  #  log10(midpoint of bin).
  
  # Gets done in LBNbiom.method(x) call above
  
  
  # Cumulative Distribution, LCD method
  x.sorted = sort(x, decreasing=TRUE)
  logSorted = log(x.sorted)
  logProp = log((1:length(x))/length(x))
  
  hLCD.lm = lm(logProp ~ logSorted)   # plot(fitsortedlog10) shows
  #  residuals not good
  
  hLCD.list = list(logSorted = logSorted,
                   logProp = logProp,
                   lm = hLCD.lm,
                   slope = hLCD.lm$coeff[2],
                   confVals = confint(hLCD.lm, "logSorted", 0.95))
  # breaks = hLlin$breaks,
  
  # MLE (maximum likelihood method) calculations.
  
  # Use analytical value of MLE b for PL model (Box 1, Edwards et al. 2007)
  #  as a starting point for nlm for MLE of b for PLB model.
  PL.bMLE = 1/( log(min(x)) - sum.log.x/length(x)) - 1
  
  PLB.minLL =  nlm(negLL.PLB,
                   p=PL.bMLE,
                   x=x,
                   n=length(x),
                   xmin=xmin,
                   xmax=xmax,
                   sumlogx=sum.log.x) #, print.level=2 )
  
  PLB.bMLE = PLB.minLL$estimate
  
  # 95% confidence intervals for MLE method.
  
  PLB.minNegLL = PLB.minLL$minimum
  
  # Values of b to test to obtain confidence interval. For the real movement data
  #  sets in Table 2 of Edwards (2011) the intervals were symmetric, so make a
  #  symmetric interval here.
  
  bvec = seq(PLB.bMLE - 0.5, PLB.bMLE + 0.5, 0.00001)
  
  PLB.LLvals = vector(length=length(bvec))  # negative log-likelihood for bvec
  for(i in 1:length(bvec))
  {
    PLB.LLvals[i] = negLL.PLB(bvec[i],
                              x=x,
                              n=length(x),
                              xmin=xmin,
                              xmax=xmax,
                              sumlogx=sum.log.x)
  }
  critVal = PLB.minNegLL  + qchisq(0.95,1)/2
  # 1 degree of freedom, Hilborn and Mangel (1997) p162.
  bIn95 = bvec[ PLB.LLvals < critVal ]
  # b values in 95% confidence interval
  PLB.MLE.bConf = c(min(bIn95), max(bIn95))
  
  if(PLB.MLE.bConf[1] == min(bvec) | PLB.MLE.bConf[2] == max(bvec))
  { dev.new()
    plot(bvec, PLB.LLvals)
    abline(h = critVal, col="red")
    warning("Need to make bvec larger - see R window")   # Could automate
  }
  
  hMLE.list = list(b = PLB.bMLE,
                   confVals = PLB.MLE.bConf)
  
  if(b.only){
    return(                # slope (or b), conf interval lower and upper
      # bounds. Note that columns are unnamed (though first
      # value seems to be named sometimes).
      list(hLlin    = c(hLlin.list$slope, hLlin.list$confVals),
           hLT      = c(hLT.list$slope, hLT.list$confVals),
           hLTplus1 = c(hLTplus1.list$slope, hLTplus1.list$confVals),
           hLBmiz   = c(hLBmiz.list$slope, hLBmiz.list$confVals),
           hLBbiom  = c(hLBNbiom.list[["unNorm.slope"]], hLBNbiom.list[["unNorm.conf"]]),
           hLBNbiom = c(hLBNbiom.list[["norm.slope"]], hLBNbiom.list[["norm.conf"]]),
           hLCD     = c(hLCD.list$slope, hLCD.list$confVals),
           hMLE     = c(hMLE.list$b, hMLE.list$confVals)))
  } else {
    return(list(hLlin.list = hLlin.list,
                hLT.list = hLT.list,
                hLTplus1.list = hLTplus1.list,
                hLBmiz.list = hLBmiz.list,
                hLBNbiom.list = hLBNbiom.list,
                hLCD.list = hLCD.list,
                hMLE.list = hMLE.list))
  }
}
