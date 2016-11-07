#' RCF and RPM Conversions
#' @description Convert RCF to RPM, or vice versa, for a given rotor.
#' @param rcf relative centrifugal force
#' @param rpm revolutions per minute
#' @param rotor a rotor object
#' 
#' @return \code{rcf2rpm} returns the rpm equilavent to the specified rotor and 
#' rcf. Likewise, \code{rpm2rcf} returns the rcf for the specified rotor and rpm.
#' 
#' @examples 
#' # Create a Beckman SW 40 Ti rotor object
#' sw40 <- rotor(model = "sw40")
#' 
#' # Calculate the rpm needed for 10,000 x g
#' rcf2rpm(10000, sw40)
#' 
#' # Calculate the rcf for an SW 40 Ti rotor spinning at 1,000 rpm
#' rpm2rcf(1000, sw40)
#' 
#' @section Warning: Check your owner's manual for the maximum rpm and rcf
#' ratings of your rotor before proceeding.
#' 

rcf2rpm <- function(rcf, rotor) {

  # Check Arguments ------------------------------------------------------------
  if(!is.numeric(rcf)) stop("rcf argument must be numeric.")
  if(class(rotor) != "rotor") stop("rotor argument is not a rotor-class object. Use rotor() to create one.")

  # Calculation ----------------------------------------------------------------
  rpm <- sqrt(rcf / (rotor[["R.ave"]] * 11.18)) * 1000

  return(rpm)
}


#' @rdname rcf2rpm
rpm2rcf <- function(rpm, rotor) {

  # Check Arguments ------------------------------------------------------------
  if(!is.numeric(rcf)) stop("rcf argument must be numeric.")
  if(class(rotor) != "rotor") stop("rotor argument is not a rotor-class object. Use rotor() to create one.")

  # Calculation ----------------------------------------------------------------
  rcf <- (rpm/1000)^2 * rotor[["R.ave"]] * 1.118

  return(rcf)
}
