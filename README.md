
# ShinyEQBayes

## Introduction

ShinyEQBayes (or EQBayes) is an R Shiny app for combining respondent
EQ5D utility values and EQVAS scores to get a joint utility measure
using Markov Chain Monte Carlo (MCMC). The modelling in the app is based
on this [Value in Health
paper](https://doi.org/10.1016/j.jval.2022.01.017). It uses the `nimble`
package to turn WinBUGS code, which the original paper used, into C++
code, making computation much faster.

## Why use EQBayes?

There are two main reasons for using this methodology to combine EQ5D
utility and EQVAS.  
1. There are prior beliefs that the sample population is somewhat
different to the population norms being used to translate EQ5D answers
into utility values. If responses from the EQVAS are notably different
to the derived utility values, this may be the case.  
2. To reduce interval widths of the health utility variable.

## Running the app offline

In some cases, for example if you’re experiencing crashing or want to
avoid uploading information to the server, it may be easier to run the
app on your local machine. To do this, simply open RStudio and install
the necessary packages if not already installed, then use the `shiny`
package to load the app from GitHub.

``` r
# Install required packages
install.packages(c("shiny", "shinydashboard", "shinycssloaders", "nimble", "tidybayes", "dplyr", "ggplot2", "here", "renv"))

# Run locally from Git
shiny::runGitHub("robinblythe/ShinyEQBayes")
```

## How to use the app

Access the hosted app at <https://aushsi.shinyapps.io/ShinyEQBayes/>, or
run it on your local machine. There is a brief introduction on the first
panel. You will need a `.csv` file with at least two columns: the EQ5D
health utility values (usually ranging from -1 to 1) and the EQVAS
values (from 0 to 100). If you have a binary variable of interest that
you want to explore, for example treatment or respondent sex, you can
use this to stratify your results.

1.  Upload your `.csv` file in the “Upload” sidebar tab.
    - Select the columns denoting your utility values, VAS, and binary
      variable, if using.
    - Check that the file upload details are correct, and inspect the
      dataset at the bottom of the tab.  
2.  Inspect your uploaded data in the “Explore” sidebar tab.
    - Check that the histograms look appropriate, and note the spread of
      the variables.
    - Use the scatterplot of paired EQ5D utility/VAS values at the
      bottom to visually inspect correlation. The notches in the axes
      are a rug plot, representing the marginal distributions of the
      variables. These variables will be used to construct the joint
      distribution of the adjusted utility value.  
3.  Select your MCMC model inputs. A sensible default based on the paper
    above has been selected, but this can be modified.
    - Number of posterior draws per chain refers to how many samples of
      the posterior distribution you want for the final model. Extra
      samples are comparably inexpensive to run with C++, but don’t go
      overboard. Between 5000-10000 per chain is probably sufficient,
      but we can come back to this later.
    - Chains represent how many times you want to re-run the model.
      Because MCMC is a random process, it is recommended to use at
      least 2 Chainz as they may explore different parts of the
      posterior distribution. A maximum of 4 chains has been set for
      this app. Each chain is relatively expensive to run so don’t go
      overboard.
    - Thinning refers to how many iterations to skip during the sampling
      process before a posterior draw is taken. This smooths out some
      variability from the sample, but there is debate around whether
      this is desirable. The default is 3.
    - Number of iterations to burn for warmup is an important input,
      because the samples from MCMC can take a while to explore the
      distribution space. Using a burn-in period reduces this
      unnecessary variation and is highly recommended.
    - Setting your seed guarantees reproducibility. It allows other
      researchers to check on the validity of your work and know whether
      your results are just a result of re-running your code until you
      get favourable results. Select a consistent number each proejct
      when using the app for research.  
4.  Hit run model without (intercept only) or (with X variable) and wait
    for the results to appear on the “Analyse” tab. You may need to
    switch panes. There should be a progress bar in the bottom right
    indicating how far along you are. It is unlikely that the app will
    freeze while in use, so please be patient. You can’t do anything
    except view the already-rendered plots while the models are
    running.  
5.  You can see your results, including the density plots of the
    posterior samples of your new joint distribution compared to the old
    ones, in the “Analyse” pane. A table underneath the plots shows the
    summary details of your results - pay particular attention to the
    ‘Width’ part of the panel to see the reduction in uncertainty from
    the joint distribution. If you want to see trace plots, the
    “Diagnostics” pane shows these for each parameter.
    - This is where the extra iterations come in. If the chains look
      densely populated around one Y value without much wandering, this
      shows that the algorithm has probably converged. If not, you
      probably need to increase the number of iterations.
    - For more complex models, there is a tradeoff between the stability
      of your results and computation time, so more complex models
      usually should be run for longer to avoid variability in your
      results. In this app, however, the models are quite simple and
      should all converge quickly.

Happy hunting!
