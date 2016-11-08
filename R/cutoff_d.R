#' Calculate the vesicle size cutoff
#' 
#' @description Calculates the maximum vesicle size that is completely sedimented at 
#' a specified time and rcf. Units are noted in parentheses
#' 
#' @param t The centrifugation time (min).
#' @param rcf The relative centrifugal force.
#' @param rotor A rotor object, created with \code(rotor()).
#' @param vesicle.density The vesicle density (g/cm^3).
#' @param solvent.density The solvent density (g/cm^3).
#' @param viscosity The viscosity of the solvent (g/(cm*s)).
#' 
#' @details Calculates the maximum vesicle size that is fully sedimented in a 
#' centrifugation run. This can be thought of as a vesicle size cufoff for complete
#' sedimentation. 
#' 
#' The default value for \code{vesicle.density} is 1.15 g/cm^3,
#' which is the most common exosome density in the literature. 
#' 
#' The default \code{solvent.density} is 1.00 g/cm^3 and the default 
#' \code{viscosity} is 0.0155 g/(cm*s), which are respectively the density and 
#' viscosity of pure water. 
#' 
#' 
#' @return The cutoff vesicle size (nm)
#' 
#' @examples 
#' # create a Beckman SW 40 Ti rotor object
#' sw40 <- rotor("sw40")
#' 
#' cutoff_d(t = 60, rcf = 10000, sw40)
#' cutoff_d(t = 60, rcf = 100000, sw40)
#' 
#' 
#' # Arguments can be of length > 1
#' cutoff_d(t = 1:100, rcf = 10000, sw40)
#' cutoff_d(t = 60, rcf = seq(100, 1000, by = 100), sw40)
#' cutoff_d(t = 1:10, rcf = seq(100, 1000, by = 100), sw40)

cutoff_d <- function(t,
                     rcf,
                     rotor,
                     vesicle.density = 1.15, 
                     solvent.density = 1.00, 
                     viscosity = 0.0155) {
    
    # Argument Checks ----------------------------------------------------------
    if(!is.numeric(t) | sum(t < 0) != 0) stop("t must be numeric and non-negative.")
    if(!is.numeric(rcf) | sum(rcf < 0) != 0) stop("rcf must be numeric and non-negative.")
    if(class(rotor) != "rotor") stop("rotor is not a rotor-class object. Use rotor() to create one.")
    if(!is.numeric(vesicle.density)) stop("vesicle.density must be numeric.")
    if(!is.numeric(solvent.density)) stop("solvent.density must be numeric.")
    if(sum(vesicle.density <= solvent.density) != 0) stop("vesicle.density must be greater than solvent.density.")
    if(!is.numeric(viscosity) | sum(viscosity < 0) != 0) stop("viscosity must be numeric and non-negative.")
    
    # Assign variables ---------------------------------------------------------
    rho <- vesicle.density - solvent.density
    rMax <- rotor[["R.max"]]
    rMin <- rotor[["R.min"]]
    rAve <- rotor[["R.ave"]]
    L <- rotor[["sed.L"]]
    
    # Do Calculations ----------------------------------------------------------
    
    switch(rotor[["type"]],
           
           # Swinging Bucket ---------------------------------------------------
           sw = {
               w2 <- rcf / rAve * 9800
               d <- sqrt(((18 * viscosity) / (w2 * (t * 60) * rho)) * log(rMax/rMin)) * 10^7
           },
           
           # Fixed Angle -------------------------------------------------------
           fa = {
               d <- sqrt((18 * viscosity * L) / (rcf * 9800 * t * 60 * rho)) * 10^7
           })
    
    return(d)
}