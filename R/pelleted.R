#' Calculate the fraction pelleted for a vesicle size
#' 
#' @description Calculates the fraction pelleted for a vesicle of a specified 
#' diameter and density when centrifuged at a specified rcf for a specified time. 
#' Units are noted in parentheses.
#' 
#' @param t time (s)
#' @param d vesicle diameter (nm)
#' @param rcf relative centrifugal force (x g)
#' @param rotor a rotor object
#' @param vesicle.density the vesicle density (g/cm^3)
#' @param solvent.density the solvent density (g/cm^3)
#' @param viscosity of the solvent (g/(cm*s))

# TODO:
# - add documentation


pelleted <- function(t, 
                     d, 
                     rcf, 
                     rotor,
                     vesicle.density = 1.15, 
                     solvent.density = 1.00, 
                     viscosity = 0.0155){
    
    # Arguements Checks --------------------------------------------------------
    if(!is.numeric(t)) stop("t argument must be numeric.")
    if(!is.numeric(d)) stop("d argument must be numeric.")
    if(!is.numeric(rcf)) stop("rcf argument must be numeric.")
    if(class(rotor) != "rotor") stop("rotor argument is not a rotor-class object. Use rotor() to create one.")
    if(!is.numeric(vesicle.density)) stop("vesicle.density argument must be numeric.")
    if(!is.numeric(solvent.density)) stop("solvent.density argument must be numeric.")
    if(!is.numeric(viscosity)) stop("viscosity argument must be numeric.")
    
    # Assign variables ---------------------------------------------------------
    rho <- vesicle.density - solvent.density
    rMax <- rotor[["R.max"]]
    rMin <- rotor[["R.min"]]
    
    switch(rotor[["type"]],
           
           # Swinging Bucket ---------------------------------------------------
           sw = {
               w2 <- (2 * rcf) / (rMax + rMin) * 9800
               lambda <- (w2 * (10^-7 * d)^2 * rho) / (18 * viscosity)
               result <- rMax / (rMax - rMin) * (1 - exp(-lambda * (t * 60)))
               
               # Because the result (fraction pelleted) must physically be between [1,0]
               result[result > 1] <- 1
           },
           
           # Fixed Angle -------------------------------------------------------
           fa = {
               L <- rotor[["FA.diameter"]] * sin(pi / 180 * rotor[["FA.angle"]])
               shift <- (rcf * 980 * (10^-7 * d)^2 * rho * (t * 60)) / (18 * viscosity * L)
               result <- (2/pi) * (asin(shift) + shift * sqrt(1 - shift^2))
               
               # Because the result (fraction pelleted) must physically be between [1,0]
               result[is.nan(result)] <- 1
           })
    

    
    return(result)
}