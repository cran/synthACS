% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_annealing.R
\name{optimize_microdata}
\alias{optimize_microdata}
\title{Optimize the selection of a micro data population.}
\usage{
optimize_microdata(
  micro_data,
  prob_name = "p",
  constraint_list,
  tolerance = round(sum(constraint_list[[1]])/2000 * length(constraint_list), 0),
  resample_size = min(sum(constraint_list[[1]]), max(500,
    round(sum(constraint_list[[1]]) * 0.005, 0))),
  p_accept = 0.4,
  max_iter = 10000L,
  seed = sample.int(10000L, size = 1, replace = FALSE),
  verbose = TRUE
)
}
\arguments{
\item{micro_data}{A \code{data.frame} of micro data observations.}

\item{prob_name}{It is assumed that observations are weighted and do not have an equal probability
of occurance. This string specifies the variable within \code{micro_data} that contains the probability
of selection.}

\item{constraint_list}{A \code{list} of constraining macro data attributes. See \code{\link{add_constraint}}}

\item{tolerance}{An integer giving the maximum acceptable loss (TAE), enabling early stopping.
Defaults to a misclassification rate of 1 individual per 1,000 per constraint.}

\item{resample_size}{An integer controlling the rate of movement about the candidate space. 
Specifically, it specifies the number of observations to change between iterations. Defaults to 
\code{0.5\%} the number of observations.}

\item{p_accept}{The acceptance probability for the Metropolis acceptance criteria.}

\item{max_iter}{The maximum number of allowable iterations. Defaults to \code{10000L}}

\item{seed}{A seed for reproducibility. See \code{\link[base]{set.seed}}}

\item{verbose}{Logical. Do you wish to see verbose output? Defaults to \code{TRUE}}
}
\description{
Optimize the candidate micro dataset such that the lowest loss against the 
macro dataset constraints is obtained. Loss is defined here as total absolute error (TAE)
and constraints are defined by the \code{constraint_list}. Optimization is done by
simulated annealing--see details.
}
\section{Details}{

Spatial microsimulation involves the study of individual-level phenomena within a specified set of
geographies in which these individuals act. It involves the creation of synthetic data to model,
via simulation, these phenomena. As a first step to simulation, an appropriate micro-level 
(ie. individual) dataset must be generated. This function creates such appropriate micro-level
datasets given a set of candidate observations and macro-level constraints.

Optimization is done via simulated annealing, where we wish to minimize the total absolute error
(TAE) between the micro-data and the macro-constraints. The annealing procedure is controlled by 
the parameters \code{tolerance}, \code{resample_size}, \code{p_accept}, and 
\code{max_iter}. Specifically, \code{tolerance} indicates the maximum allowable TAE between the
output micro-data and the macro-constraints within a given \code{max_iter} allowable iterations 
to converge. \code{resample_size} and \code{p_accept} control movement about the candidate space. 
Specfically, \code{resample_size} controls the jump size between neighboring 
candidates and \code{p_accept} controls the hill-climbing rate for exiting local minima.

Please see the references for a more detailed discussion of the simulated annealing procedure.
}

\examples{
\dontrun{
## assumes you have micro_synthetic object named test_micro and constraint_list named c_list
opt_data <- optimize_microdata(test_micro, "p", c_list, max_iter= 10, resample_size= 500, 
              p_accept= 0.01, verbose= FALSE)
}
}
\references{
Ingber, Lester. "Very fast simulated re-annealing." Mathematical and computer 
modelling 12.8 (1989): 967-973.

Metropolis, Nicholas, et al. "Equation of state calculations by fast computing 
machines." The journal of chemical physics 21.6 (1953): 1087-1092.

Szu, Harold, and Ralph Hartley. "Fast simulated annealing." Physics letters A 122.3 
(1987): 157-162.
}
