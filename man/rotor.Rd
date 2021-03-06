% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rotor.R
\name{rotor}
\alias{list_rotors}
\alias{rotor}
\title{Create a rotor object}
\usage{
rotor(model = "custom", type = "sw", R.min, R.max, FA.angle = NA,
  FA.diameter = NA)

list_rotors()
}
\arguments{
\item{model}{Use a custom or preset rotor.
Setting model to "custom" allows use of the other parameters. Use
\code{list_rotors()} to see preset model choices.}

\item{type}{Swinging-bucket ("sw") or fixed-angle ("fa").}

\item{R.min}{The minimum radius of the rotor in mm.}

\item{R.max}{The maximum radius of the rotor in mm.}

\item{FA.angle}{The tube angle in degrees for fixed-angle rotors. Necessary to calculate
the sedimentation path length.}

\item{FA.diameter}{The tube diameter in mm for fixed-angle rotors. Necessary to
calculate the sedimentation path length.}
}
\description{
Constructor function to create new rotor class objects.
}
\details{
Rotor objects are lists that contain the physical parameters of specific
rotors. The \code{rotor()} function creates a new rotor object for use with other evrspin
functions.

While a user may specify parameters individually using \code{model = "custom"}, there
are a number of preset rotors that can be selected through the \code{model} parameter.
Use \code{list_rotors()} to return a dataframe containing the preset rotors.
Note that if \code{model != "custom"}, all other parameters are overidden by
the preset values.
}
\examples{
# List preset rotors
list_rotors()

# Create a Beckman SW 40 Ti rotor object
rotor(model = "sw40")

# Create your own swinging-bucket rotor object.
# Note that FA.angle and FA.diameter are not needed for swinging-bucket rotors.
rotor(model = "custom", type = "sw", R.min = 10, R.max = 25)

# Create your own fixed-angle rotor object.
rotor(model = "custom", type = "fa", R.min = 10, R.max = 15, FA.angle = 30, FA.diameter = 10)

# Other parameters are overidden if model != "custom"
rotor(model = "sw40", type = "fa", R.min = 10, R.max = 15, FA.angle = 30, FA.diameter = 10)

}

