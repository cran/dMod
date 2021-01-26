#' Search for symmetries in the loaded model
#' 
#' @description This function follows the method published in [1].
#' @description The function calls a python script via reticulate. Usage problems might occur when different python versions are used. The script was written and tested for python 2.7.12, sympy 0.7.6.
#' @description The code is currently ported to Python 3. 
#' 
#' @param f object containing the ODE for which \code{as.eqnvec()} is defined
#' @param obsvect vector of observation functions
#' @param prediction vector containing prediction to be tested
#' @param initial vector containing initial values
#' @param ansatz type of infinitesimal ansatz used for the analysis (uni, par, multi)
#' @param pMax maximal degree of infinitesimal ansatz
#' @param inputs specify the input variables
#' @param fixed variables to concider fixed
#' @param cores maximal number of cores used for the analysis
#' @param allTrafos do not remove transformations with a common parameter factor
#' @return NULL
#' 
#' @references [1]
#' \url{https://journals.aps.org/pre/abstract/10.1103/PhysRevE.92.012920}
#' 
#' @examples
#' \dontrun{
#' eq <- NULL
#' eq <- addReaction(eq, "A", "B", "k1*A")
#' eq <- addReaction(eq, "B", "A", "k2*B")
#' 
#' observables <- eqnvec(Aobs = "alpha * A")
#' 
#' symmetryDetection(eq, observables)
#' 
#' }
#' @export
symmetryDetection <- function(f, obsvect = NULL, prediction = NULL,
                              initial = NULL, ansatz = 'uni', pMax = 2, inputs = NULL, fixed = NULL,
                              cores = 1, allTrafos = FALSE){
  
  f <- as.eqnvec(f)
  
  f <- as.character(lapply(1:length(f), function(i)
    paste(names(f)[i],'=',f[i])))
  
  obsvect <- as.character(lapply(1:length(obsvect), function(i)
    paste(names(obsvect)[i],'=',obsvect[i])))
  
  if (!is.null(prediction)) {
    prediction <- as.character(lapply(1:length(prediction), function(i)
      paste(names(prediction)[i],'=',prediction[i])))
  }
  
  if (!is.null(initial)) {
    initial <- as.character(lapply(1:length(initial), function(i)
      paste(names(initial)[i],'=',initial[i])))
  }
  reticulate::use_python("/usr/bin/python", required = TRUE)
  
  
  reticulate::source_python(system.file("code/polyClass.py", package = "dMod"))
  reticulate::source_python(system.file("code/functions.py", package = "dMod"))
  reticulate::source_python(system.file("code/readData.py", package = "dMod"))
  reticulate::source_python(system.file("code/buildSystem.py", package = "dMod"))
  reticulate::source_python(system.file("code/checkPredictions.py", package = "dMod"))
  reticulate::source_python(system.file("code/symmetryDetection.py", package = "dMod"))
  
  symmetryDetectiondMod(
    f, obsvect,
    prediction, initial, ansatz, pMax, inputs, fixed, cores,
    allTrafos
  )

}
