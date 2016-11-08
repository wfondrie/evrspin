#' Calculate the fraction pelleted for a vesicle size
#' 
#' @description Calculates the fraction pelleted for a vesicle of a specified 
#' diameter and rcf for a specified time. Units are noted in parentheses.
#' 
#' @param t The centrifugation time (min).
#' @param d The vesicle diameter (nm).
#' @param rcf The relative centrifugal force.
#' @param rotor A rotor object, created with \code(rotor()).
#' @param vesicle.density The vesicle density (g/cm^3).
#' @param solvent.density The solvent density (g/cm^3).
#' @param viscosity The viscosity of the solvent (g/(cm*s)).
#' 
#' @details Calculates the fraction of a vesicle population that is pelleted
#' during a cetrifugation run at the specified time and rcf. Though the original
#' equation allows for fractions greater than 1 (for swinging bucket rotors) or 
#' may become undefined (for fixed-angle rotors) the maximum value \code{pelleted()}
#' will return is 1 due to the physical meaning of 100% sedimentation. 
#' 
#' The default value for \code{vesicle.density} is 1.15 g/cm^3,
#' which is the most common exosome density in the literature. 
#' 
#' The default \code{solvent.density} is 1.00 g/cm^3 and the default 
#' \code{viscosity} is 0.0155 g/(cm*s), which are respectively the density and 
#' viscosity of pure water. 
#' 
#' @return The fraction of a vesicle population pelleted. Is always numeric 
#' between [0, 1].
#' 
#' @examples 
#' # create a Beckman SW 40 Ti rotor object
#' sw40 <- rotor("sw40")
#' 
#' pelleted(t = 60, d = 100, rcf = 10000, sw40)
#' pelleted(t = 60, d = 100, rcf = 100000, sw40)
#' 
#' 
#' # Arguments can be of length > 1
#' pelleted(t = 1:100, d = 100, rcf = 10000, sw40)
#' #' pelleted(t = 60, d = 100:110, rcf = 10000, sw40)
#' pelleted(t = 60, d = 100, rcf = seq(100, 1000, by = 100), sw40)
#' pelleted(t = 1:10, d = 100:110, rcf = seq(100, 1000, by = 100), sw40)

pelleted <- function(t, 
                     d, 
                     rcf, 
                     rotor,
                     vesicle.density = 1.15, 
                     solvent.density = 1.00, 
                     viscosity = 0.0155){
    
    # Arguments Checks ---------------------------------------------------------
    if(!is.numeric(t) | sum(t < 0) != 0) stop("t must be numeric and non-negative.")
    if(!is.numeric(d) | sum(d < 0) != 0) stop("d must be numeric and non-negative.")
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
    L <- rotor[["sed.L"]]
    
    switch(rotor[["type"]],
           
           # Swinging Bucket ---------------------------------------------------
           sw = {
               w2 <- (2 * rcf) / (rMax + rMin) * 9800
               lambda <- (w2 * (10^-7 * d)^2 * rho) / (18 * viscosity)
               result <- rMax / L * (1 - exp(-lambda * (t * 60)))
               
               # Because the result (fraction pelleted) must physically be between [1,0]
               result[result > 1] <- 1
           },
           
           # Fixed Angle -------------------------------------------------------
           fa = {
               shift <- (rcf * 980 * (10^-7 * d)^2 * rho * (t * 60)) / (18 * viscosity * L)
               result <- (2/pi) * (asin(shift) + shift * sqrt(1 - shift^2))
               
               # Because the result (fraction pelleted) must physically be between [1,0]
               result[is.nan(result)] <- 1
           })
    
    return(result)
}