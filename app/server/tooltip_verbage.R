tooltip_text <- list()
tooltip_text$feas_legend <-  HTML(paste(h6("E1 -- High environmental suitability"), 
                              p("Typically, no environmental limiting conditions for establishment and growth across the full range of site series conditions."),
                              h6("E2 -- Moderate environmental suitability"), 
                              p("Some site series conditions have low suitability for the species as reflected in regeneration challenges and/or fair growth."),
                              h6("E3 -- Low environmental suitability"),
                              p("Only specific site series conditions are suitable for the species or generally poor growth and survival across the site series."), 
                              collapse = "<br />" ))

tooltip_text$model_agree <-  "Percent of models predicting each suitability class"
tooltip_text$cfrg <-  "Suitability rating and preferred/acceptable classes from the Chief Foresters Reference Guide"
tooltip_text$es <-  "Baseline historic environmental suitability"
tooltip_text$cciss_estab <-  "Current climate establishment suitability (1961 - 2040)"
tooltip_text$cciss_mature <-  "Future period (2021-2100) suitability"
tooltip_text$cciss_pa <-  "Preliminary preferred/acceptable"
tooltip_text$trends <-  "Ratio of models indicting improving/stable trends/ models declining or unsuitable"

tooltip_text$upload_csv <- HTML(paste("Upload a csv file with columns sitename, latitude, longitude, and (optionally) siteseries with your points of interest."))
tooltip_text$bgc_click <- HTML(paste("Click on map to use preselected points across an entire BGC subzone/variant or only BGCs within a Forest District"))
tooltip_text$select_points <- HTML(paste("Click on the map to add one or more points or use 'Enter New' to manually add lat/long coordinates"))