#' Rotor class constructors

rotor <- function(model = "custom",
                  type = "SW",
                  R.min,
                  R.max,
                  FA.angle,
                  FA.diameter) {
    
    # Parameter checking -------------------------------------------------------
    
    # Custom Rotor -------------------------------------------------------------
    if(model == "custom") {
        rotor <- structure(list(model = model,
                                type = type,
                                R.min = R.min,
                                R.max = R.max,
                                FA.angle = FA.angle,
                                FA.diameter = FA.diameter),
                           class = "rotor")
    }
    
    # Preset Rotors ------------------------------------------------------------
    
    # Swinging Bucket Rotors
    
    ## Beckman SW 41 Ti
    if(model == "SW 41") {
        rotor <- structure(list(model = "SW 41",
                                type = "SW",
                                R.min = 67.4,
                                R.max = 153.1,
                                FA.angle = NA,
                                FA.diameter = NA),
                           class = "rotor")
    }
    
    
    # Calculate R.avg ----------------------------------------------------------
    #rotor <- append(rotor, list(R.ave))
}
