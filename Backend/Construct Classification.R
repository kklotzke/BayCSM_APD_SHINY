# Construct classification structure 

ConstructCM <- function(f1, f2, f8, PF) {
  
  if (any(c(f1, f2, f8, PF))) {
    p <- 48 # Number of items  
  
    # Full classification matrix
    u <- matrix(0, nrow = 14, ncol = p)
    u[1, ] <- 1 # General math ability (f1)
    u[2, 1:24] <- 1 # Informal mathematics (f2.1)
    u[3, 25:48] <- 1 # Formal mathematics (f2.2)
    u[4, 1:7] <- 1 # Numbering (f8.1)
    u[5, 8:13] <- 1 # Number comparisons (f8.2)
    u[6, 14:18] <- 1 # Calculation (f8.3)
    u[7, 19:24] <- 1 # Concepts (f8.4)
    u[8, 25:30] <- 1 # Numeral literacy (f8.5)
    u[9, 31:35] <- 1 # Number facts (f8.6)
    u[10, 36:43] <- 1 # Calculation (f8.7)
    u[11, 44:48] <- 1 # Concepts (f8.8)
    u[12, c(1:24, 25, 31, 37, 43)] <- 1 # Presentation format text (PF.1)
    u[13, c(seq(26, 48, by = 6), seq(27, 48, by = 6), seq(28, 48, by = 6))] <- 1 # Presentation format text + image (PF.2)
    u[14, c(seq(29, 48, by = 6), seq(30, 48, by = 6))] <- 1 # Presentation format image (PF.3)
  
    # Selected factor structures
    fs <- list(NULL, NULL, NULL, NULL)
    fs.names <- c("f1", "f2", "f8", "PF")
    if (f1) {
      fs[[1]] <- matrix(1, nrow = 1, ncol = p, dimnames = list(c("f1"), NULL))
    }
    if (f2) {
      fs[[2]] <- matrix(NA, nrow = 2, ncol = p, dimnames = list(paste("f2.", 1:2, sep = ""), NULL))
      fs[[2]][1, ] <- u[2, ]
      fs[[2]][2, ] <- u[3, ]
    }
    if (f8) {
      fs[[3]] <- matrix(NA, nrow = 8, ncol = p, dimnames = list(paste("f8.", 1:8, sep = ""), NULL))
      fs[[3]][1, ] <- u[4, ]
      fs[[3]][2, ] <- u[5, ]
      fs[[3]][3, ] <- u[6, ]
      fs[[3]][4, ] <- u[7, ]
      fs[[3]][5, ] <- u[8, ]
      fs[[3]][6, ] <- u[9, ]
      fs[[3]][7, ] <- u[10, ]
      fs[[3]][8, ] <- u[11, ]
    }
    if (PF) {
      fs[[4]] <- matrix(NA, nrow = 3, ncol = p, dimnames = list(paste("PF.", 1:3, sep = ""), NULL))
      fs[[4]][1, ] <- u[12, ]
      fs[[4]][2, ] <- u[13, ]
      fs[[4]][3, ] <- u[14, ]
    }
      
    
    # Construct classification matrix 
    u1 <- matrix(NA , nrow = 0, ncol = p)
    names <- c()
    for (ff in 1:4) {
      if (!is.null(fs[[ff]])) {
        u1 <- rbind(u1, fs[[ff]]) 
        names <- c(names, fs.names[ff])
      }
    }
    
    # Construct model identificator 
    modelid <- NULL
    if (length(names) == 1) {
      if (names[1] == "f1") 
        modelid <- "Unidimensional (f1)"
      else
        modelid <- paste0("Multidimensional (", names[1],")")
    }
    if (length(names) == 2) {
      modelid <- paste0("Bi-factor (", paste0(names, collapse = "+"), ")")
    }
    else if (length(names) == 3) {
      modelid <- paste0("Tri-factor (", paste0(names, collapse = "+"), ")")
    }
    else if (length(names) == 4) {
      modelid <- paste0("Quad-factor (", paste0(names, collapse = "+"), ")")
    }
    
    return (list("u1" = u1, "Nt" = nrow(u1), "names" = names, "modelid" = modelid))
  }
  else {
    return (list("u1" = NULL, "Nt" = 0, "names" = NULL, "modelid" = "Not specified"))
  }
    
}