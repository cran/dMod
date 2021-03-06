% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction.R
\name{Xf}
\alias{Xf}
\title{Model prediction function for ODE models without sensitivities.}
\usage{
Xf(
  odemodel,
  forcings = NULL,
  events = NULL,
  condition = NULL,
  optionsOde = list(method = "lsoda")
)
}
\arguments{
\item{odemodel}{Object of class \link{odemodel}.}

\item{forcings, }{see \link{Xs}}

\item{events, }{see \link{Xs}}

\item{condition}{either NULL (generic prediction for any condition) or a character, denoting
the condition for which the function makes a prediction.}

\item{optionsOde}{list with arguments to be passed to odeC() for the ODE integration.}
}
\description{
Interface to get an ODE 
into a model function \code{x(times, pars, forcings, events)} returning ODE output.
It is a reduced version of \link{Xs}, missing the sensitivities.
}
\details{
Can be used to integrate additional quantities, e.g. fluxes, by adding them to \code{f}. 
All quantities that are not initialised by pars 
in \code{x(..., forcings, events)} are initialized with 0. For more details and
the return value see \link{Xs}.
}
