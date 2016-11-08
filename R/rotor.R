#' Create a rotor object
#'
#' @description Constructor function to create new rotor class objects.
#'
#' @param model Use a custom or preset rotor.
#' Setting model to "custom" allows use of the other parameters. Use
#' \code{list_rotors()} to see preset model choices.
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
#' are a number of preset rotors that can be selected through the \code{model} parameter.
#' Use \code{list_rotors()} to return a dataframe containing the preset rotors.
#' Note that if \code{model != "custom"}, all other parameters are overidden by
#' the preset values.
#'
#'@examples
#' # List preset rotors
#' list_rotors()
#'
#' # Create a Beckman SW 40 Ti rotor object
#' rotor(model = "sw40")
#'
#' # Create your own swinging-bucket rotor object.
#' # Note that FA.angle and FA.diameter are not needed for swinging-bucket rotors.
#' rotor(model = "custom", type = "sw", R.min = 10, R.max = 25)
#'
#' # Create your own fixed-angle rotor object.
#' rotor(model = "custom", type = "fa", R.min = 10, R.max = 15, FA.angle = 30, FA.diameter = 10)
#'
#' # Other parameters are overidden if model != "custom"
#' rotor(model = "sw40", type = "fa", R.min = 10, R.max = 15, FA.angle = 30, FA.diameter = 10)
#'
#' @export

rotor <- function(model = "custom",
                  type = "sw",
                  R.min,
                  R.max,
                  FA.angle = NA,
                  FA.diameter = NA) {

    # Argument Checks ----------------------------------------------------------
    if(model == "custom") {
        if(type != "sw" & type != "fa") stop("type must be either \"sw\" or \"fa\"." )
        if(!is.numeric(R.min)) stop("R.min must be numeric.")
        if(R.min < 0) stop("R.min cannot be less than 0.")
        if(!is.numeric(R.max)) stop("R.max must be numeric.")
        if(R.max <= 0) stop("R.max must be positive and numeric.")
        if(R.max <= R.min) stop("R.max must be greater than R.min")

        if(type == "fa") {
            if(!is.numeric(FA.angle)) stop("FA.angle must be numeric.")
            if(FA.angle > 90 | FA.angle < 0) stop("FA.angle must be between 0 and 90.")
            if(!is.numeric(FA.diameter)) stop("FA.diameter must be numeric.")
            if(FA.diameter <= 0) stop("FA.diameter must be positive.")
        }
    }


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
           },

           # If neither "custom" nor any preset above:
           stop("model does not match any preset rotors. Use list_rotors() to see preset models or use model = \"custom\" to specify your own.")
    ) # end switch()

    # Calculate properties of rotors -------------------------------------------

    # Sedimentation Length
    switch(rotor[["type"]],
           sw = {
               sed.L <- rotor[["R.max"]] - rotor[["R.min"]]
           },
           fa = {
               sed.L <- rotor[["FA.diameter"]] / cos(pi / 180 * rotor[["FA.angle"]])
           })

    # Average Radius
    R.ave <- (rotor[["R.max"]] + rotor[["R.min"]]) / 2

    rotor <- structure(append(rotor, list(R.ave = R.ave, sed.L = sed.L), 4),
                       class = "rotor")
    return(rotor)
}


#' @rdname rotor
#' @export
#'
list_rotors <- function() {
    return(data.frame(model = c("sw41",
                                "sw40",
                                "sw28",
                                "mls50",
                                "type45",
                                "type60",
                                "type70",
                                "f452415",
                                "tla110"),
                      info = c("Beckman SW 41 Ti",
                               "Beckman SW 40 Ti",
                               "Beckman SW 28",
                               "Beckman MLS-50",
                               "Beckman Type 45 Ti",
                               "Beckman Type 60 Ti",
                               "Beckman Type 70 Ti",
                               "Eppendorf F-45-24-15",
                               "Beckman TLA 110"),
                      type = c(rep("swinging-bucket (sw)", 4),
                               rep("fixed-angle (fa)", 5))
    )
    )
}
