#' Calculate the maximum vesicle size to be fully sedimented
#' 
#' @description Calculates the maximum vesicle size that is completely sedimented at 
#' a specified time and density at a specified rcf. Units are noted in parentheses
#' 
#' @param t time (s)
#' @param rcf relative centrifugal force (x g)
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
    if(!is.numeric(t)) stop("t must be numeric.")
    if(!is.numeric(rcf)) stop("rcf argument must be numeric.")
    if(class(rotor) != "rotor") stop("rotor argument is not a rotor-class object. Use rotor() to create one.")
    if(!is.numeric(vesicle.density)) stop("vesicle.density must be numeric")
    if(!is.numeric(solvent.density)) stop("solvent.density must be numeric")
    if(!is.numeric(viscosity)) stop("viscosity must be numeric")
    
    # Assign variables ---------------------------------------------------------
    rho <- vesicle.density - solvent.density
    rMax <- rotor[["R.max"]]
    rMin <- rotor[["R.min"]]
    
    # Do Calculations ----------------------------------------------------------
    
    switch(rotor[["type"]],
           
           # Swinging Bucket ---------------------------------------------------
           sw = {
               w2 <- (2 * rcf) / (rMax + rMin) * 9800
               d <- sqrt(((18 * viscosity) / (w2 * (t * 60) * rho)) * log(rMax/rMin)) * 10^7
           },
           
           # Fixed Angle -------------------------------------------------------
           fa = {
               L <- rotor[["FA.diameter"]] * sin(pi / 180 * rotor[["FA.angle"]])
               d <- sqrt((18 * viscosity * L) / (rcf * 980 * t * 60 * rho)) * 10^7
           })
    
    return(d)
}