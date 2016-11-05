#' Calculate the time to sediment a vesicle
#' 
#' @description Calculates the time needed for complete sedimentation of a specified
#' size and density at a specified rcf. Units are noted in parentheses.
#' 
#' @param d vesicle diameter (nm)
#' @param rcf relative centrifugal force (x g)
#' @param rotor a rotor object
#' @param vesicle.density the vesicle density (g/cm^3)
#' @param solvent.density the solvent density (g/cm^3)
#' @param viscosity of the solvent (g/(cm*s))
#' 
# TODO:
# - Add more documentation

sed_time <- function(d, 
                     rcf, 
                     rotor,
                     vesicle.density = 1.15, 
                     solvent.density = 1.00, 
                     viscosity = 0.0155) {
    
    # Arguements Checks --------------------------------------------------------
    if(!is.numeric(d)) stop("d must be numeric.")
    if(!is.numeric(rcf)) stop("rcf argument must be numeric.")
    if(class(rotor) != "rotor") stop("rotor argument is not a rotor-class object. Use rotor() to create one.")
    if(!is.numeric(vesicle.density)) stop("vesicle.density must be numeric")
    if(!is.numeric(solvent.density)) stop("solvent.density must be numeric")
    if(!is.numeric(viscosity)) stop("viscosity must be numeric")
    
    # Assign variables ---------------------------------------------------------
    rho <- vesicle.density - solvent.density
    rMax <- rotor[["R.max"]]
    rMin <- rotor[["R.min"]]
    
    # Do Calculation -----------------------------------------------------------
    switch(rotor[["type"]],
           
           # Swinging Bucket ---------------------------------------------------
           sw = {
               w2 <- (2 * rcf) / (rMax + rMin) * 9800
               ((18 * viscosity) / (w2 * (10^-7 * d)^2 * rho)) * log(rMax/rMin) / 60
           },
           
           # Fixed Angle -------------------------------------------------------
           fa = {
               L <- (rMax - rMin) * sin(pi / 180 * rotor[["FA.angle"]])
               (18 * n * L) / (rcf * 980 * (10^-7 * d)^2 * rho) / 60
           })
    
}