#' Calculate the maximum vesicle size to be fully sedimented
#' 
#' @description Calculates the maximum vesicle size that is completely sedimented at 
#' a specified time and density at a specified rcf. Units are noted in parentheses
#' 
#' @param t time (s)
#' @param rcf relative centrifugal force
#' @param rotor a rotor object
#' @param vesicle.density the vesicle density (g/cm^3)
#' @param solvent.density the solvent density (g/cm^3)
#' @param viscosity of the solvent (g/(cm*s))
#' 


# TODO:
# - Add more documentation

cutoff_d <- function(t,
                     rcf,
                     rotor,
                     vesicle.density = 1.15, 
                     solvent.density = 1.00, 
                     viscosity = 0.0155) {
    
    # Arguements Checks --------------------------------------------------------
    if(!is.numeric(t) | t < 0) stop("t must be numeric and non-negative.")
    if(!is.numeric(rcf) | rcf < 0) stop("rcf must be numeric and non-negative.")
    if(class(rotor) != "rotor") stop("rotor is not a rotor-class object. Use rotor() to create one.")
    if(!is.numeric(vesicle.density)) stop("vesicle.density must be numeric.")
    if(!is.numeric(solvent.density)) stop("solvent.density must be numeric.")
    if(!is.numeric(viscosity) | viscosity < 0) stop("viscosity must be numeric and non-negative.")
    
    # Assign variables ---------------------------------------------------------
    rho <- vesicle.density - solvent.density
    if(rho <= 0) stop("vesicle.density must be greater than solvent.density")
    rMax <- rotor[["R.max"]]
    rMin <- rotor[["R.min"]]
    L <- rotor[["sed.L"]]
    
    # Do Calculations ----------------------------------------------------------
    
    switch(rotor[["type"]],
           
           # Swinging Bucket ---------------------------------------------------
           sw = {
               w2 <- (2 * rcf) / (rMax + rMin) * 9800
               d <- sqrt(((18 * viscosity) / (w2 * (t * 60) * rho)) * log(rMax/rMin)) * 10^7
           },
           
           # Fixed Angle -------------------------------------------------------
           fa = {
               d <- sqrt((18 * viscosity * L) / (rcf * 980 * t * 60 * rho)) * 10^7
           })
    
    return(d)
}