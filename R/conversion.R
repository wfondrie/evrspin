#' RCF Conversion
#' @description Converts RCF to RPM for a given rotor.
#' @param rcf relative centrifugal force in x g
#' @param rotor a rotor object
#' @return rotor velocity in rotations-per-minute (RPM)

# TODO:
# - add documentation

rcf2rpm <- function(rcf, rotor) {

  # Check Arguments ------------------------------------------------------------
  if(!is.numeric(rcf)) stop("rcf argument must be numeric.")
  if(class(rotor) != "rotor") stop("rotor argument is not a rotor-class object. Use rotor() to create one.")

  # Calculation ----------------------------------------------------------------
  rpm <- sqrt(rcf / (rotor[["R.ave"]] * 11.18)) * 1000

  return(rpm)
}

#' RPM Conversion
#' @description Converts RPM to RCF for given rotor.
#' @param rpm velocity of the rotor in rotations-per-minute
#' @param rotor a rotor object
#' @return relative centrifugal force (RCF) of the rotor in x g

rpm2rcf <- function(rpm, rotor) {

  # Check Arguments ------------------------------------------------------------
  if(!is.numeric(rcf)) stop("rcf argument must be numeric.")
  if(class(rotor) != "rotor") stop("rotor argument is not a rotor-class object. Use rotor() to create one.")

  # Calculation ----------------------------------------------------------------
  rcf <- (rpm/1000)^2 * rotor[["R.ave"]] * 1.118

  return(rcf)
}
