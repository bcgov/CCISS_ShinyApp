analog_novelty <- function(clim.targets, clim.analogs, label.targets, label.analogs, vars,
                           clim.point = NULL, 
                           clim.icvs = NULL, label.icvs = NULL, weight.icv = 0.5, sigma = TRUE,
                           analog.focal = NULL, threshold = 0.95, pcs = NULL, logVars = TRUE, 
                           plotScree = FALSE, 
                           plot2d = FALSE, plot2d.pcs = cbind(c(1,2,3,4), c(2,3,4,5)), 
                           plot3d = FALSE, plot3d.pcs=c(1,2,3), biplot = TRUE, 
                           plot3d.candidates = FALSE){
  
  analogs <- if(is.null(analog.focal)) unique(label.targets) else analog.focal # list of analogs to loop through
  novelty <- rep(NA, length(label.targets)) # initiate a vector to store the sigma dissimilarities
  
  for(analog in analogs){ # loop through all of the analogs used to describe the target climates. 
    clim.analog <- clim.analogs[label.analogs==analog, ..vars]
    clim.target <- clim.targets[label.targets==analog, ..vars]
    if(!is.null(clim.point)) clim.point <- clim.point[, ..vars]
    if(!is.null(clim.icvs)){
      setDT(clim.icvs)
      clim.icv <- clim.icvs[label.icvs==analog, ..vars]
    }
    if(plot3d.candidates) clim.analogs.all <- clim.analogs[, ..vars]
    
    ## data cleaning
    clim.analog <- clim.analog[complete.cases(clim.analog)] # remove rows without data
    clim.analog <- clim.analog[, .SD, .SDcols = which(sapply(clim.analog, function(x) var(x, na.rm = TRUE) > 0))]  # Remove zero-variance columns
    clim.target <- clim.target[, .SD, .SDcols = names(clim.analog)]
    if(!is.null(clim.point)) clim.point <- clim.point[, .SD, .SDcols = names(clim.analog)]
    if(!is.null(clim.icvs)) clim.icv <- clim.icv[complete.cases(clim.icv)]
    if(!is.null(clim.icvs)) clim.icv <- clim.icv[, .SD, .SDcols = names(clim.analog)]
    if(plot3d.candidates){
      label.analogs <- label.analogs[complete.cases(clim.analogs.all)]
      clim.analogs.all <- clim.analogs.all[complete.cases(clim.analogs.all)]
      clim.analogs.all <- clim.analogs.all[, .SD, .SDcols = names(clim.analog)]
    }
    
    ## log-transform ratio variables
    if(logVars){
      clim.analog <- logVars(clim.analog, zero_adjust = TRUE)
      clim.target <- logVars(clim.target, zero_adjust = TRUE)
      if(!is.null(clim.point)) clim.point <- logVars(clim.point, zero_adjust = TRUE)
      if(!is.null(clim.icvs)) clim.icv <- logVars(clim.icv, zero_adjust = TRUE)
      if(plot3d.candidates) clim.analogs.all <- logVars(clim.analogs.all, zero_adjust = TRUE)
      
      ## remove variables with non-finite values in the target population (this is an edge case that occurs when the target population has a variable (typically CMD) with only zeroes)
      clim.target <- clim.target[, lapply(.SD, function(x) if (all(is.finite(x))) x else NULL)]
      clim.analog <- clim.analog[, .SD, .SDcols = names(clim.target)]
      if(!is.null(clim.point)) clim.point <- clim.point[, .SD, .SDcols = names(clim.target)]
      if(!is.null(clim.icvs)) clim.icv <- clim.icv[, .SD, .SDcols = names(clim.target)]
      if(plot3d.candidates) clim.analogs.all <- clim.analogs.all[, .SD, .SDcols = names(clim.target)]
    }
    
    ## scale the data to the variance of the analog, since this is what we will ultimately be measuring the M distance in. 
    clim.mean <- clim.analog[, lapply(.SD, mean, na.rm = TRUE)]
    clim.sd <- clim.analog[, lapply(.SD, sd, na.rm = TRUE)]
    clim.analog[, (names(clim.analog)) := lapply(names(clim.analog), function(col) {
      (get(col) - unlist(clim.mean)[col]) / unlist(clim.sd)[col]
    })]
    clim.target[, (names(clim.target)) := lapply(names(clim.target), function(col) {
      (get(col) - unlist(clim.mean)[col]) / unlist(clim.sd)[col]
    })]
    if(!is.null(clim.point)) clim.point[, (names(clim.point)) := lapply(names(clim.point), function(col) {
      (get(col) - unlist(clim.mean)[col]) / unlist(clim.sd)[col]
    })]
    if(!is.null(clim.icvs)) clim.icv[, (names(clim.icv)) := lapply(names(clim.icv), function(col) {
      (get(col) - unlist(clim.icv[, lapply(.SD, mean, na.rm = TRUE)])[col]) / unlist(clim.sd)[col] # subtract mean of ICV to centre the ICV on zero. 
    })]
    if(plot3d.candidates) clim.analogs.all[, (names(clim.analogs.all)) := lapply(names(clim.analogs.all), function(col) {
      (get(col) - unlist(clim.mean)[col]) / unlist(clim.sd)[col]
    })]
    
    ## PCA on pooled target and analog
    s <- sample(1:dim(clim.target)[1], dim(clim.analog)[1], replace = TRUE) # select a random sample of the target population to match the analog points. bootstrap if target population is smaller than analog points
    clim.target.sample <- clim.target[s,]
    pca <- prcomp(rbind(clim.analog, clim.target.sample), scale=FALSE)
    pcs.analog <- data.table(predict(pca, clim.analog))
    pcs.target <- data.table(predict(pca, clim.target))
    if(!is.null(clim.point)) pcs.point <- data.table(predict(pca, clim.point))
    if(!is.null(clim.icvs)) pcs.icv <- data.table(predict(pca, clim.icv))
    
    if(is.null(pcs)){
      ## select number of pcs
      cumvar <- cumsum(pca$sdev^2 / sum(pca$sdev^2)) # vector of cumulative variance explained
      pcs <- which(cumvar >= threshold)[1]
      if(pcs<3) pcs <- 3
    }
    
    ## z-standardize the pcs to the variance of the analog. this is necessary for a metric that can be translated into sigma values. 
    weight.analog <- 1 - weight.icv
    pcs.mean.analog <- pcs.analog[, lapply(.SD, mean, na.rm = TRUE)]
    pcs.sd.analog <- pcs.analog[, lapply(.SD, sd, na.rm = TRUE)]
    if(!is.null(clim.icvs)) pcs.sd.icv <- pcs.icv[, lapply(.SD, sd, na.rm = TRUE)]
    if(!is.null(clim.icvs)) pcs.sd.combined <- weight.analog * pcs.sd.analog + weight.icv * pcs.sd.icv
    pcs.sd.use <- if(!is.null(clim.icvs)) pcs.sd.combined else pcs.sd.analog
    pcs.analog[, (names(pcs.analog)) := lapply(names(pcs.analog), function(col) {
      (get(col) - unlist(pcs.mean.analog)[col]) / unlist(pcs.sd.use)[col]
    })]
    pcs.target[, (names(pcs.target)) := lapply(names(pcs.target), function(col) {
      (get(col) - unlist(pcs.mean.analog)[col]) / unlist(pcs.sd.use)[col]
    })]
    if(!is.null(clim.point)) pcs.point[, (names(pcs.point)) := lapply(names(pcs.point), function(col) {
      (get(col) - unlist(pcs.mean.analog)[col]) / unlist(pcs.sd.use)[col]
    })]
    if(!is.null(clim.icvs)) pcs.icv[, (names(pcs.icv)) := lapply(names(pcs.icv), function(col) {
      (get(col) - unlist(pcs.icv[, lapply(.SD, mean, na.rm = TRUE)])[col]) / unlist(pcs.sd.use)[col] # separately centering on the ICV mean because sometime the ICV is not centered on the centroid, and we want it to be. 
    })]
    
    ## create a combined covariance matrix for spatial variation and ICV
    cov.analog <- var(pcs.analog[, 1:pcs])
    cov.icv <- if (!is.null(clim.icvs)) var(pcs.icv[, 1:pcs]) else NULL
    if (!is.null(cov.icv)) {
      cov.combined <- weight.analog * cov.analog + weight.icv * cov.icv
    } else {
      cov.combined <- cov.analog
    }
    
    ## Mahalanobis distance and sigma dissimilarity
    md <- (mahalanobis(pcs.target[,1:pcs], rep(0, pcs), cov.combined))^0.5
    p <- pchi(md,pcs) # percentiles of the M distances on the chi distribution with degrees of freedom equaling the dimensionality of the distance measurement (PCs)
    q <- qchi(p,1) # values of the chi percentiles on a standard half-normal distribution (chi distribution with one degree of freedom)
    q[!is.finite(q)] <- 8 # set infinite values to 8 sigma (outside the decimal precision of pchi) 
    q[is.na(p)] <- NA # reset NA values as NA
    
    ## populate the novelty vector
    novelty[label.targets==analog] <- if(sigma) q else md
    
  } # end of the for-loop
  
  ## Plots for the final iteration of the for loop
  
  # Color Scheme for sigma novelty
  breakseq <- c(0,4,8)
  breakpoints <- c(seq(breakseq[1], breakseq[3], 0.01),199); length(breakpoints)
  ColScheme <- c(colorRampPalette(c("gray90", "gray50", "#FFF200", "#CD0000", "black"))(length(breakpoints)))
  
  ## Scree plot
  if(plotScree){
    par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(1.75,0.25,0))
    a <- apply(predict(pca, clim.analog), 2, sd)
    b <- apply(predict(pca, clim.target), 2, sd)
    if(!is.null(clim.icvs)) c <- apply(predict(pca, clim.icv), 2, sd)
    diff <- abs(apply(predict(pca, clim.target), 2, mean) - apply(predict(pca, clim.analog), 2, mean))
    plot(0, xlim=c(1,length(a)), ylim=c(0,max(c(a,b, diff))*1.02), yaxs="i", col="white", tck=-0.005,
         xlab="Principal Component (PC)", ylab = "Standard Deviation")
    rect(pcs+0.5, -99, 99, 99, col = "grey95", lty=2)
    points(a, pch=21, bg="dodgerblue", cex=1.6)
    points(b, bg="grey", pch=21, cex=1.3)
    if(!is.null(clim.icvs)) points(c, bg="black", pch=21, cex=1)
    points(diff, col="black", pch=17, cex=1.3)
    text(pcs+0.5, max(c(a,b, diff)), paste0("Truncation at ", pcs, " PCs"), pos=4)
    s <- if(!is.null(clim.icvs)) 1:4 else 1:3
    legend("topright", title=analog, 
           legend=c("Analog", "Target", "Separation of means", "ICV")[s], 
           pt.bg=c("dodgerblue", "grey", NA, NA)[s], 
           col = c("black", "black", "black", "black")[s], 
           pt.cex=c(1.6,1.3,1.3, 1)[s], 
           pch=c(21, 21, 17, 16)[s], 
           bty="n")
    box()
  }
  
  ## 2D scatterplot
  if(plot2d){
    par(mfrow=c(2,2), mar=c(3,3,1,1), mgp=c(1.75,0.25,0))
    for(i in 1:4){
      a <- predict(pca, clim.analog)[, plot2d.pcs[i,]]
      b <- predict(pca, clim.target)[, plot2d.pcs[i,]]
      b <- sweep(b, 2, apply(a, 2, mean), '-') # shift the target data so that the analog centroid is at zero. this is done at a later stage than the pca in the distance calculation.
      a <- sweep(a, 2, apply(a, 2, mean), '-') # centre the analog centroid on zero. this is done at a later stage than the pca in the distance calculation.
      plot(a, col="dodgerblue", xlim=range(c(a[,1], b[,1])), ylim=range(c(a[,2], b[,2])), asp=1, tck=0.01)
      points(b, bg=ColScheme[cut(q, breakpoints)], pch=21, cex=1.5)
      if(!is.null(clim.icvs)){
        c <- predict(pca, clim.icv)[, plot2d.pcs[i,]]
        c <- sweep(c, 2, apply(c, 2, mean), '-') # centre the ICV on the analog centroid. this is done at a later stage than the pca in the distance calculation. 
        points(c, col="black", pch=16, cex=1)
      }
      points(a, col="dodgerblue", pch=16)
      mtext(paste(analog, "\n", pcs, "PCs"), line=-2.5, adj = 0.05, )
      mtext(paste0("(", letters[i], ")"), side=3, line=-1, adj = -0.065, font=2)
    }
  }
  
  ## 3D scatterplot
  if(plot3d){
    
    # revert to the raw pcs (centered on the analog centroid), because standardization obscures the shape of the analog distribution
    a <- predict(pca, clim.analog)
    b <- predict(pca, clim.target)
    b <- sweep(b, 2, apply(a, 2, mean), '-') # shift the target data so that the analog centroid is at zero. this is done at a later stage than the pca in the distance calculation.
    if(plot3d.candidates){
      d <- predict(pca, clim.analogs.all)
      d <- sweep(d, 2, apply(a, 2, mean), '-') # shift the candidate data so that the analog centroid is at zero. 
      e <- aggregate(d, by=list(label.analogs), FUN=mean)
      e <- e[is.finite(e$PC1),]
      label.analogs.mean <- e$Group.1
      e <- e[,-1]
    }
    a <- sweep(a, 2, apply(a, 2, mean), '-') # centre the analog centroid on zero. this is done at a later stage than the pca in the distance calculation.
    
    b_colors <- ColScheme[cut(q, breakpoints)] # Define colors for points in 'b'
    
    # Create the 3D scatterplot
    plot <- plot_ly() %>%
      add_trace(
        x = a[, plot3d.pcs[1]], y = a[, plot3d.pcs[2]], z = a[, plot3d.pcs[3]],
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = "dodgerblue", opacity = 1),
        hoverinfo = "none", # Turn off hover labels
        name = "Analog Points"
      ) %>%
      add_trace(
        x = b[, plot3d.pcs[1]], y = b[, plot3d.pcs[2]], z = b[, plot3d.pcs[3]],
        type = "scatter3d", mode = "markers",
        marker = list(size = 6, color = b_colors, opacity = 1),
        hoverinfo = "none", # Turn off hover labels
        name = "Target Points"
      ) 
    # Add ICV points if they exist
    if(!is.null(clim.icvs)) {
      c <- predict(pca, clim.icv)
      c <- sweep(c, 2, apply(c, 2, mean), '-') # centre the ICV on the analog centroid. this is done at a later stage than the pca in the distance calculation. 
      plot <- plot %>%
        add_trace(
          x = c[, plot3d.pcs[1]], y = c[, plot3d.pcs[2]], z = c[, plot3d.pcs[3]],
          type = "scatter3d", mode = "markers",
          marker = list(size = 4, color = "black", opacity = 1),
          hoverinfo = "none", # Turn off hover labels
          name = "ICV"
        )
    }
    # Add selected point if it exists
    if(!is.null(clim.point)) {
      f <- predict(pca, clim.point)
      f <- sweep(f, 2, apply(a, 2, mean), '-') # shift the target data so that the analog centroid is at zero. this is done at a later stage than the pca in the distance calculation.
      plot <- plot %>%
        add_trace(
          x = f[, plot3d.pcs[1]], y = f[, plot3d.pcs[2]], z = f[, plot3d.pcs[3]],
          type = "scatter3d", mode = "markers",
          marker = list(size = 20, color = "black", opacity = 1, symbol = 'cross'),
          hoverinfo = "none", # Turn off hover labels
          name = "Selected location"
        )
    }
    # Add candidate analogs
    if(plot3d.candidates){
      data("zones_colours_ref")
      zone <- rep(NA, length(label.analogs.mean))
      for(i in zones_colours_ref$classification){ zone[grep(i,label.analogs.mean)] <- i }
      # zone <- factor(zone, zones_colours_ref$classification)
      zone_colours <- as.character(zones_colours_ref$colour[match(zone, zones_colours_ref$classification)]) 
      zone_colours[is.na(zone_colours)] <- "#808080"
      # zone_colours <- factor(zone_colours, zones_colours_ref$colour)
      
      plot <- plot %>%
        add_trace(
          x = d[, plot3d.pcs[1]], y = d[, plot3d.pcs[2]], z = d[, plot3d.pcs[3]],
          type = "scatter3d", mode = "markers",
          marker = list(size = 2, color = "#cccccc", opacity = 0.35),
          hoverinfo = "none", # Turn off hover labels
          name = "All analogs"
        ) %>%
        add_trace(
          x = e[, plot3d.pcs[1]], y = e[, plot3d.pcs[2]], z = e[, plot3d.pcs[3]],
          type = "scatter3d", mode = "markers+text",
          marker = list(size = 5, color = zone_colours, opacity = 1),
          # marker = list(size = 3, color = "#000000", opacity = 0.5),
          text = label.analogs.mean, # Vector of labels corresponding to points in e
          textposition = "right",
          textfont = list(size = 8, color = "#666666"),
          hoverinfo = "none", # Turn off hover labels
          name = "BGC centroids"
        )
    }
    
    # Add biplot lines
    if(biplot) {
      loadings <- pca$rotation[, plot3d.pcs]
      scale_factor <- max(abs(c(a, b))) * 2
      scaled_loadings <- loadings * scale_factor
      for (i in 1:nrow(scaled_loadings)) {
        plot <- plot %>%
          add_trace(
            x = c(0, scaled_loadings[i, 1]),
            y = c(0, scaled_loadings[i, 2]),
            z = c(0, scaled_loadings[i, 3]),
            type = "scatter3d",
            mode = "lines+text",
            line = list(color = "black", width = 2),
            text = rownames(scaled_loadings)[i],
            textposition = "middle center",
            showlegend = FALSE, 
            name = paste("Loading:", rownames(scaled_loadings)[i])
          )
      }
    }
    plot <- plot %>%
      layout(
        scene = list(
          xaxis = list(title = paste0("PC", plot3d.pcs[1]), showspikes = FALSE),
          yaxis = list(title = paste0("PC", plot3d.pcs[2]), showspikes = FALSE),
          zaxis = list(title = paste0("PC", plot3d.pcs[3]), showspikes = FALSE)
        ),
        title = list(text = paste(analog, "\nNovelty in", pcs, "PCs"), x = 0.05)
      )
    # Display the plot
    print(plot)
  }
  return(novelty)
}
