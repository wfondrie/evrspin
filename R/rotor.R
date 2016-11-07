#' Create a rotor object
#'
#' @description Constructor function to create new rotor class objects.
#'
#' @param model custom or pre-specified rotor.
#' Setting model to "custom" allows use of the other parameters. See details for
#' other model choices.
#'
#' @param type Swinging-bucket ("sw") or fixed-angle ("fa").
#'
#' @param R.min The minimum radius of the rotor in mm.
#'
#' @param R.max The maximum radius of the rotor in mm.
#'
#' @param FA.angle The tube angle in degrees for fixed-angle rotors. Necessary to calculate
#' the sedimentation path length.
#'
#' @param FA.diameter The tube diameter in mm for fixed-angle rotors. Necessary to
#' calculate the sedimentation path length.
#'
#'
#' @details Rotor objects are lists that contain the physical parameters of specific
#' rotors. The \code{rotor()} function creates a new rotor object for use with other evrspin
#' functions.
#'
#' While a user may specify parameters individually using \code{model = "custom"}, there
#' are a number of pre-specified rotors that can be selected through the \code{model} parameter.
#' Note that if \code{model != "custom"}, all other parameters are ignored.
#'
#' \bold{Supported Models:}
#' \itemize{
#'      \item Beckman SW 41 Ti ("sw41")
#' }
#'
#'

# TODO:
# - sed.L calculation is incorrect!
# - Add more rotors
# - Add argument checks

rotor <- function(model = "custom",
                  type = "sw",
                  R.min,
                  R.max,
                  FA.angle,
                  FA.diameter) {
    
    # Parameter checking -------------------------------------------------------
    
    
    
    # Custom Rotor -------------------------------------------------------------
    switch(model,
           custom = {
               rotor <- list(model = model,
                             type = type,
                             R.min = R.min,
                             R.max = R.max,
                             FA.angle = FA.angle,
                             FA.diameter = FA.diameter)
           },
           
           
           # Swinging Bucket Rotors --------------------------------------------
           ## Beckman SW 41 Ti
           sw41 = {
               rotor <- list(model = "sw41",
                             type = "sw",
                             R.min = 67.4,
                             R.max = 153.1,
                             FA.angle = NA,
                             FA.diameter = NA)
           },
           
           sw40 = {
               rotor <- list(model = "sw40",
                             type = "sw",
                             R.min = 66.7,
                             R.max = 158.8,
                             FA.angle = NA,
                             FA.diameter = NA)
           },
           
           sw28 = {
               rotor <- list(model = "sw28",
                             type = "sw",
                             R.min = 75.3,
                             R.max = 161,
                             FA.angle = NA,
                             FA.diameter = NA)
           },
           
           mls50 = {
               rotor <- list(model = "mls50",
                             type = "sw",
                             R.min = 47.5,
                             R.max = 95.8,
                             FA.angle = NA,
                             FA.diameter = NA)
           },
           
           # Fixed-Angle Rotors ------------------------------------------------
           type45 = {
               rotor <- list(model = "type45",
                             type = "fa",
                             R.min = 35.9,
                             R.max = 103.8,
                             FA.angle = 24,
                             FA.diameter = 38)
           },
           
           type60 = {
               rotor <- list(model = "type60",
                             type = "fa",
                             R.min = 36.9,
                             R.max = 89.9,
                             FA.angle = 23.5,
                             FA.diameter = 25)
           },
           
           type70 = {
               rotor <- list(model = "type70",
                             type = "fa",
                             R.min = 39.5,
                             R.max = 91.9,
                             FA.angle = 23,
                             FA.diameter = 15)
           },
           
           f452415 = {
               rotor <- list(model = "f452415",
                             type = "fa",
                             R.min = 54,
                             R.max = 82,
                             FA.angle = 45,
                             FA.diameter = 11)
           },
           
           tla110 = {
               rotor <- list(model = "tla110",
                             type = "fa",
                             R.min = 26,
                             R.max = 48.5,
                             FA.angle = 28,
                             FA.diameter = 13)
           })
    
    # Calculate properties of rotors -------------------------------------------
    switch(rotor[["type"]],
           sw = {
               sed.L <- rotor[["R.max"]] - rotor[["R.min"]]
           },
           fa = {
               sed.L <- rotor[["FA.diameter"]] * cos(pi / 180 * rotor[["FA.angle"]])
           })
    t
    rotor <- structure(append(rotor, list(sed.L = sed.L)), class = "rotor")
    return(rotor)
}
