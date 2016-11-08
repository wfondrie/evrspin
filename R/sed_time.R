#' Calculate the time to completely sediment a vesicle
#'
#' @description Calculates the time needed for complete sedimentation of vesicles with specified
#' size and rcf. Units are noted in parentheses.
#'
#' @param d The vesicle diameter (nm).
#' @param rcf The relative centrifugal force.
#' @param rotor A rotor object, created with \code{rotor()}.
#' @param vesicle.density The vesicle density (g/cm^3).
#' @param solvent.density The solvent density (g/cm^3).
#' @param viscosity The viscosity of the solvent (g/(cm*s)).
#'
#' @details Calculates the centrifugation time needed to completely sediment a
#' vesicle population at the provided rcf. This is the theoretical maximum time
#' for a vesicle to traverse the sedimentation path length or \code{sed.L} of a
#' rotor object.
#'
#' The default value for \code{vesicle.density} is 1.15 g/cm^3,
#' which is the most common exosome density in the literature.
#'
#' The default \code{solvent.density} is 1.00 g/cm^3 and the default
#' \code{viscosity} is 0.0155 g/(cm*s), which are respectively the density and
#' viscosity of pure water.
#'
#' @return The time to completely sediment a vesicle population (min).
#'
#' @examples
#' # create a Beckman SW 40 Ti rotor object
#' sw40 <- rotor("sw40")
#'
#' sed_time(d = 100, rcf = 10000, sw40)
#' sed_time(d = 100, rcf = 100000, sw40)
#'
#'
#' # Arguments can be of length > 1
#' sed_time(d = 100:110, rcf = 10000, sw40)
#' sed_time(d = 100, rcf = seq(100, 1000, by = 100), sw40)
#' sed_time(d = 100:110, rcf = seq(100, 1000, by = 100), sw40)




sed_time <- function(d,
                     rcf,
                     rotor,
                     vesicle.density = 1.15,
                     solvent.density = 1.00,
                     viscosity = 0.0155) {

    # Arguments Checks ---------------------------------------------------------
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
    L <- rotor[["sed.L"]]

    # Do Calculation -----------------------------------------------------------
    switch(rotor[["type"]],

           # Swinging Bucket ---------------------------------------------------
           sw = {
               w2 <- (2 * rcf) / (rMax + rMin) * 9800
               t <- ((18 * viscosity) / (w2 * (10^-7 * d)^2 * rho)) * log(rMax/rMin) / 60
           },

           # Fixed Angle -------------------------------------------------------
           fa = {
               t <- (18 * viscosity * L) / (rcf * 980 * (10^-7 * d)^2 * rho) / 60
           })

    return(t)
}
