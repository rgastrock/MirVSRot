# Exponential Function -----

# fit a single exponential to learning data, with two parameters:
# - a learning rate
# - an asymptote (for incomplete learning)

# will replace asymptotic decay, but should do the same thing
# except that's it's slightly closer to an actual exponential
# and uses it behind the scenes, so that:
# it should run faster
# people can use the output for maths


#' @title Run an exponential function given parameters and a reach
#' deviation schedule. Errors decay exponentially.
#' @param par A named vector with the model parameter (see details).
#' @param timepoints An integer indicating the number of trials (N), or a vector
#' with N trial numbers (these can have missing values or be fractions). If an
#' integer, the timepoints at which the exponential will be evaluated is:
#' 0, 1 ... N-2, N-1
#' @param mode String: "learning" or "washout", sets the function's direction.
#' @param setN0 NULL or number, if the asymptote is known, it can be set here.
#' @return A data frame with two columns: `timepoint` and `output`, and N rows,
#' so that each row has the output of the modeled process on each trial.
#' @description This function is part of a set of functions to fit and
#' evaluate an exponential decay model with asymptote.
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - lambda: learning rate
#' - N0: asymptote
#' @examples
#' # write example!
#' @export
exponentialModel <- function(par, timepoints, mode='learning', setN0=NULL) {
  
  if (length(timepoints) == 1) {
    timepoints <- c(0:(timepoints-1))
  }
  
  if (is.numeric(setN0)) {
    par['N0'] = setN0
  }
  
  if (mode == 'learning') {
    output = par['N0'] - ( par['N0'] * (1-par['lambda'])^timepoints )
  }
  if (mode == 'washout') {
    output = par['N0'] * (par['lambda'])^timepoints
  }
  
  return(data.frame(trial=timepoints,
                    output=output))
  
}

#' @title Get the MSE between an exponential and a series of reach deviations.
#' @param par A named numeric vector with the model parameters (see
#' exponentialModel).
#' @param signal A numeric vector of length N with reach deviations matching
#' the perturbation schedule.
#' @param timepoints Either an integer with the number of trials (N) or a vector
#' with N trial numbers (this can have missing values or fractions). The 
#' exponential will be evaluated at those timepoints.
#' @param mode String: "learning" or "washout", sets the function's direction.
#' @return A float: the mean squared error between the total model output and
#' the reach deviations.
#' @param setN0 NULL or number, if the asymptote is known, it can be set here.
#' @description This function is part of a set of functions to fit and
#' evaluate an exponential function to describe a series of reach deviations.
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - lambda: the learning rate
#' - N0: the asymptote
#' @examples
#' # write example?
#' @export
exponentialMSE <- function(par, signal, timepoints=c(0:(length(signal)-1)), mode='learning', setN0=NULL) {
  
  MSE <- mean((exponentialModel(par, timepoints, mode=mode, setN0=setN0)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

#' @title Fit an asymptotic decay model to reach deviations.
#' @param signal A vector of length N with reach deviation data. These should
#' start around 0 and go up (ideally they are baselined).
#' @param timepoints NULL or a vector of length N with the timepoints at which
#' to evaluate the exponential. If NULL, the N values in `signal` are placed
#' at: 0, 1, ... N-2, N-1.
#' @param mode A string, one of "learning" or "washout". For "learning" the
#' signal starts at 0 and increases with exponentially decaying errors, going
#' towards asymptote ("N0"), and for "washout" it starts at "N0" and approaches
#' 0 over time.
#' @param gridpoints Number of values for rate of change and asymptote, that
#' are tested in a grid.
#' @param gridfits Number of best results from gridsearch that are used for
#' optimizing a fit.
#' @param setN0 NULL or number, if the asymptote is known, it can be set here.
#' @return A named numeric vector with the optimal parameter that fits a simple
#' rate model to the data as best as possible, with these elements:
#' - lambda: the rate of change in the range [0,1]
#' - N0: the asymptote (or starting value) in the unit of the signal
#' @description This function is part of a set of functions to fit and
#' evaluate a simple exponential function to reach deviations.
#' @details
#' ?
#' @examples
#' # write example!
#' @import optimx
#' @export
exponentialFit <- function(signal, timepoints=length(signal), mode='learning', gridpoints=11, gridfits=10, setN0=NULL) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  asymptoteRange <- c(-1,2)*max(abs(signal), na.rm=TRUE)
  
  # define the search grid:
  if (is.numeric(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals)
    lo <- c(0)
    hi <- c(1)
  }
  if (is.null(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals,
                              'N0'     = parvals * diff(asymptoteRange) + asymptoteRange[1] )
    lo <- c(0,asymptoteRange[1])
    hi <- c(1,asymptoteRange[2])
  }
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=exponentialMSE, MARGIN=c(1), signal=signal, timepoints=timepoints, mode=mode, setN0=setN0)
  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=exponentialMSE,
                            method     = 'L-BFGS-B',
                            lower      = lo,
                            upper      = hi,
                            timepoints = timepoints,
                            signal     = signal,
                            mode       = mode,
                            setN0      = setN0 ) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  if (is.null(setN0)) {
    winpar <- unlist(win[1:2])
  } else {
    winpar <- c( 'lambda' = unlist(win[1]), 
                 'N0'     = setN0)
    names(winpar) <- c('lambda', 'N0')
  }
  
  # return the best parameters:
  return(winpar)
  
}
