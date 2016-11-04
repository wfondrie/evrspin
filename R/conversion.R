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

rpm2rcf <- function(rpm, rotor) {

  # Check Arguments ------------------------------------------------------------
  if(!is.numeric(rcf)) stop("rcf argument must be numeric.")
  if(class(rotor) != "rotor") stop("rotor argument is not a rotor-class object. Use rotor() to create one.")

  # Calculation ----------------------------------------------------------------
  rcf <- (rpm/1000)^2 * rotor[["R.ave"]] * 1.118

  return(rcf)
}
