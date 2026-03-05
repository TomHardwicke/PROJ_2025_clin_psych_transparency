# This script will run a precision analysis to help with sample size planning 
# when you wish to estimate a proportion with confidence intervals
# This code was written for personal use — reuse at your own risk!

# Load libraries ---------------------------------

library(tidyverse)

# Define functions ---------------------------------

# custom ggplot theme

theme_custom <- function () { 
  theme_grey(base_size = 18, base_family = "sans") %+replace% 
    theme(
      # plot margin
      plot.margin = unit(rep(0.5, 4), "cm"),
      # plot background and border
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      # grid lines
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dotted', color = "#cbcbcb"), 
      panel.grid.minor = element_blank(),
      # axis ticks and lines
      axis.ticks = element_blank(),
      axis.line = element_line(),
      axis.title.y = element_text(hjust = 0, vjust = 2, angle = 90), # Adjust the vertical position of the y-axis label
      # title, subtitle and caption
      #plot.title = element_text(size = 20, face = "bold", colour = "#757575", hjust = 0),
      # legend
      legend.position = "top",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(size = 12, colour = "#757575"),
      legend.text.align = 0,
      legend.text = element_text(size = 14, colour = "#757575"),
      # facetting
      strip.background = element_rect(fill = "transparent", colour = NA),
      strip.text = element_text(size = 12, face = "bold", colour = "#757575", hjust = 0)
    )
}

# Define function for computing margin of error (MOE) for a confidence interval. The MOE is half the width of the confidence interval.
# If the population size is known a finite population correction can be applied.
# Otherwise, if the population size in unknown, no correction is applied (the default).
# The function can compute MOE for either a Wald or Wilson confidence interval (defaults to Wilson as it appears to be superior, see Newcombe, 1998)
# A 95% confidence interval is assumed.
marginOfError <- function(expectedProportion,sampleSize,confidenceLevel=.95,populationSize=Inf,type='wilson',report=T){
  n <- sampleSize
  N <- populationSize
  
  if(type=='wilson'){ # if computing Wilson CIs
    successCount <- n*expectedProportion
    CI <- prop.test(successCount, n, correct = F, conf.level = confidenceLevel)$conf.int # computes CI. Yate's continuity correction is off by default
    MOE <- (CI[2] - CI[1])/2 # to get the MOE, subtract lowerbound of CI from upperbound of CI and divide by two
    
  }else if(type=='wald'){ # if computing wald CIs
    P <- expectedProportion
    alpha <- (1 - confidenceLevel) / 2 # convert confidence level to alpha
    Z <- qnorm(alpha, lower.tail=FALSE) # convert alpha to Z
    MOE <- Z*sqrt(P*(1-P)/n) # compute margin of error for Wald confidence intervals 
  }
  
  if(N != Inf){ # if user has specified a population size
    FPC <- sqrt((N-n)/(N-1)) # calculate a finite population correction (FPC) factor
    MOE <- MOE*FPC # adjust the MOE with the FPC
  }
  
  if(report){ # if user has asked for a report
    print(paste0(
      'The margin of error of a 95% ', type,' confidence interval for an expected proportion of ',expectedProportion,
      ' and a sample size of ',sampleSize,' is ', round(MOE,3)))
  }
  
  moe_object <- list(moe = MOE, target_sample_size = sampleSize, conf_interval_type = type)
  
  return(moe_object)
}

# define function to plot a precision curve for given proportion and sample size (95% confidence interval assumed).
precisionCurve <- function(expectedProportion,sampleSize,confidenceLevel=.95,populationSize=Inf,type='wilson'){
  
  sample_size_vector <- seq(1:(sampleSize*2)) # create a vector of sample sizes for X axis (by default we will show a curve for 2x the specified sample size)
  moe_vector <- NA # pre-load a variable to store moes
  
  for(i in sample_size_vector){ # using a for loop as its more comprehensible 
    moe_object <- marginOfError(
      sampleSize = i,
      expectedProportion = expectedProportion,
      confidenceLevel = confidenceLevel,
      populationSize = populationSize,
      type = type,
      report = F # don't print a report
    )
    moe_vector <- c(moe_vector, moe_object$moe) # append new moe to moe vector
  }
  
  moe_vector <- moe_vector[!is.na(moe_vector)] # remove the NA value created when moe_vector was pre-loaded
  df <- data.frame(sampleSize = sample_size_vector,precision = moe_vector)
  
  # build plot
  plot <- ggplot(data = df, aes(x = sampleSize, y = precision)) +
    geom_line(colour = 'black', linewidth = .75) +
    theme_classic() +
    ylab('Precision (margin of error)') +
    xlab('Sample size') +
    scale_x_continuous(expand = c(0.0, 0), limits = c(0,max(sample_size_vector))) +
    scale_y_continuous(expand = c(0.0, 0), limits = c(0,round(max(moe_vector)+.1,1)), breaks = seq(0,round(max(moe_vector)+.1,1),0.1)) +
    theme(panel.grid.major = element_line(colour="grey97", size=0.5))
  
  return(plot)
}

# Run a precision analysis ---------------------------------

# Run the code below to compute a margin of error for given expected proportion and target sample size
moe_object1 <- marginOfError(
  expectedProportion = .5, # Specify the anticipated proportion. Use 0.5 for the most conservative sample size estimate (see Gelman & Hill, 2006, p. 442).
  sampleSize = 100, # Specify the target sample size
  populationSize = Inf, # If population size is known, we can apply a finite population correction, otherwise use Inf
  confidenceLevel = .95, # specify the level of the confidence interval
  type = 'wilson')

# Lets get a second moe for a different target sample size (so we can display on a precision curve)
moe_object2 <- marginOfError(
  expectedProportion = .5, # Specify the anticipated proportion. Use 0.5 for the most conservative sample size estimate (see Gelman & Hill, 2006, p. 442).
  sampleSize = 200, # Specify the target sample size
  populationSize = Inf, # If population size is known, we can apply a finite population correction, otherwise use Inf
  confidenceLevel = .95, # specify the level of the confidence interval
  type = 'wilson')

# Lets get a third moe for a different target sample size (so we can display on a precision curve)
moe_object3 <- marginOfError(
  expectedProportion = .5, # Specify the anticipated proportion. Use 0.5 for the most conservative sample size estimate (see Gelman & Hill, 2006, p. 442).
  sampleSize = 300, # Specify the target sample size
  populationSize = Inf, # If population size is known, we can apply a finite population correction, otherwise use Inf
  confidenceLevel = .95, # specify the level of the confidence interval
  type = 'wilson')

# Now plot a precision curve showing precision (margin of error) as a function of sample size.
# We'll annotate the plot with the parameters above

precisionCurve(expectedProportion =.5, # Specify the anticipated proportion. Use 0.5 for the most conservative sample size estimate (see Gelman & Hill, 2006, p. 442).
               sampleSize = 300, # Specify the target sample size
               populationSize = Inf, # If population size is known, we can apply a finite population correction, otherwise use Inf
               confidenceLevel = .95, # specify the level of the confidence interval
               type = 'wilson' # specify the type of confidence interval (wald or wilson)
) +
  theme_custom() +
  # this code annotates the precision curve with the moes computed above
  annotate("segment", x = moe_object1$target_sample_size, xend = moe_object1$target_sample_size, y = 0, yend = moe_object1$moe, colour = "purple", size=1, alpha=0.6) +
  annotate("segment", x = 0, xend = moe_object1$target_sample_size, y = moe_object1$moe, yend = moe_object1$moe, colour = "purple", size=1, alpha=0.6) +
  annotate("label", x = 20, y = moe_object1$moe, colour = "purple", size=4, alpha=1, label = round(moe_object1$moe,3)) +
  annotate("label", x = moe_object1$target_sample_size, y = 0.02, colour = "purple", size=4, alpha=1, label = moe_object1$target_sample_size) + 
  
  annotate("segment", x = moe_object2$target_sample_size, xend = moe_object2$target_sample_size, y = 0, yend = moe_object2$moe, colour = "skyblue", size=1, alpha=0.6) +
  annotate("segment", x = 0, xend =  moe_object2$target_sample_size, y = moe_object2$moe, yend = moe_object2$moe, colour = "skyblue", size=1, alpha=0.6) +
  annotate("label", x = 20, y = moe_object2$moe, colour = "skyblue", size=4, alpha=1, label = round(moe_object2$moe,3)) +
  annotate("label", x = moe_object2$target_sample_size, y = 0.02, colour = "skyblue", size=4, alpha=1, label = moe_object2$target_sample_size) +
  
  annotate("segment", x = moe_object3$target_sample_size, xend = moe_object3$target_sample_size, y = 0, yend = moe_object3$moe, colour = "orange", size=1, alpha=0.6) +
  annotate("segment", x = 0, xend =  moe_object3$target_sample_size, y = moe_object3$moe, yend = moe_object3$moe, colour = "orange", size=1, alpha=0.6) +
  annotate("label", x = 20, y = moe_object3$moe, colour = "orange", size=4, alpha=1, label = round(moe_object3$moe,3)) +
  annotate("label", x = moe_object3$target_sample_size, y = 0.02, colour = "orange", size=4, alpha=1, label = moe_object3$target_sample_size) 

# References  ---------------------------------

# Gelman, A., & Hill, J. (2007). Data analysis using regression and multilevel/hierarchical models. Cambridge University Press.
# Newcombe R.G. (1998). Two-Sided Confidence Intervals for the Single Proportion: Comparison of Seven Methods. Statistics in Medicine, 17, 857–872. doi:10.1002/(SICI)1097-0258(19980430)17:8<857::AID-SIM777>3.0.CO;2-E.
# Rothman, K. J., & Greenland, S. (2018). Planning study size based on precision rather than power. Epidemiology, 29(5), 599–603. https://doi.org/10.1097/EDE.0000000000000876