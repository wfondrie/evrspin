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

# TODO: Add more rotors

rotor <- function(model = "custom",
                  type = "sw",
                  R.min,
                  R.max,
                  FA.angle,
                  FA.diameter) {

  # Parameter checking -------------------------------------------------------

  # Custom Rotor -------------------------------------------------------------
  if(model == "custom") {
    rotor <- list(model = model,
                  type = type,
                  R.min = R.min,
                  R.max = R.max,
                  FA.angle = FA.angle,
                  FA.diameter = FA.diameter)
  }

  # Preset Rotors ------------------------------------------------------------

  # Swinging Bucket Rotors

  ## Beckman SW 41 Ti
  if(model == "sw41") {
    rotor <- list(model = "sw41",
                  type = "sw",
                  R.min = 67.4,
                  R.max = 153.1,
                  FA.angle = NA,
                  FA.diameter = NA)
  }


  # Calculate R.avg ----------------------------------------------------------
  R.ave <- (rotor$R.min + rotor$R.max)/2
  rotor <- structure(append(rotor, list(R.ave = R.ave), 4),
                     class = "rotor")
  return(rotor)
}
