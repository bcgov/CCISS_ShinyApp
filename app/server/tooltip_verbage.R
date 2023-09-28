tooltip_text <- list()
tooltip_text$feas_legend <-  HTML(paste(h6("E1 -- High environmental suitability"), 
                              p("Typically, no environmental limiting conditions for establishment and growth across the full range of site series conditions."),
                              h6("E2 -- Moderate environmental suitability"), 
                              p("Some site series conditions have low suitability for the species as reflected in regeneration challenges and/or fair growth."),
                              h6("E3 -- Low environmental suitability"),
                              p("Only specific site series conditions are suitable for the species or generally poor growth and survival across the site series."), 
                              collapse = "<br />" ))

tooltip_text$model_agree <-  HTML(paste(p("Percent of models predicting each feasibility class"), 
                                        collapse = "<br />" ))

tooltip_text$cfrg <-  HTML(paste(p("Suitability rating and preferred/acceptable classes from the Cheif Foresters Reference Guide"), 
                                        collapse = "<br />" )) 

tooltip_text$es <-  HTML(paste(p("Baseline historic environmental suitability"), 
                                 collapse = "<br />" )) 

tooltip_text$cciss_estab <-  HTML(paste(p("Current climate establishment feasibility"), 
                               collapse = "<br />" )) 
tooltip_text$cciss_mature <-  HTML(paste(p("Future period (2021-2100) feasibility"), 
                                        collapse = "<br />" )) 
tooltip_text$cciss_pa <-  HTML(paste(p("Preliminary preferred/acceptable"), 
                                         collapse = "<br />" )) 
tooltip_text$trends <-  HTML(paste(p("Ratio of models indicting improving/stable trends/ models declining or unsuitable"), 
                                         collapse = "<br />" )) 

