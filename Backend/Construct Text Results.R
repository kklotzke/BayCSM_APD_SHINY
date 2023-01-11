# Construct result text

ConstructTextResults <- function (list.scale = NULL, list.itemc = NULL) {
  results_text_scalepos <- results_text_scaleneg <- results_text_itemcpos <- results_text_itemcneg <- NULL
  vec_scalepos_id <- vec_scaleneg_id <- vec_itemcpos_id <- vec_itemcneg_id <- NULL
  
  if (length(list.scale) > 0) {
    vec_scalepos_id <- character(0) 
    vec_scaleneg_id <- character(0) 
    for (ii in 1:length(list.scale)) {
      df <- list.scale[[ii]]
      vec_scalepos_id <- c(vec_scalepos_id, as.character(df[which(df$"CI.ind" == 1), "Factor.id"]))
      vec_scaleneg_id <- c(vec_scaleneg_id, as.character(df[which(df$"CI.ind" == -1), "Factor.id"]))
    }
  }
  
  if (length(list.itemc) > 0) {
    vec_itemcpos_id <- character(0) 
    vec_itemcneg_id <- character(0) 
    for (ii in 1:length(list.itemc)) {
      df <- list.itemc[[ii]]
      vec_itemcpos_id <- c(vec_itemcpos_id, as.character(df[which(df$"CI.ind" == 1), "Factor.id"]))
      vec_itemcneg_id <- c(vec_itemcneg_id, as.character(df[which(df$"CI.ind" == -1), "Factor.id"]))
    }
  }
  
  if (!is.null(vec_scalepos_id) & (length(vec_scalepos_id) > 0)) {
    if (length(vec_scalepos_id) == 1)
      results_text_scalepos <- paste0("Scores on scale dimension [", paste(vec_scalepos_id, collapse = " "), "] are statistically distinct from other scores in the model.")
    else
      results_text_scalepos <- paste0("Scores on scale dimensions [", paste(vec_scalepos_id, collapse = " "), "] are statistically distinct from other scores in the model.")
  }
  
  if (!is.null(vec_scaleneg_id) & (length(vec_scaleneg_id) > 0)) {
    if (length(vec_scaleneg_id) == 1)
      results_text_scaleneg <- paste0("Inconsistent response behaviour found on scale dimension [", paste(vec_scaleneg_id, collapse = " "), "].")
    else 
      results_text_scaleneg <- paste0("Inconsistent response behaviour found on scale dimensions [", paste(vec_scaleneg_id, collapse = " "), "].")
  }
  
  if (!is.null(vec_itemcpos_id) & (length(vec_itemcpos_id) > 0)) {
    if (length(vec_itemcpos_id) == 1)
      results_text_itemcpos <- paste0("Item characteristic [", paste(vec_itemcpos_id, collapse = " "), "] explains a statistically important part of score variance.")
    else
      results_text_itemcpos <- paste0("Item characteristics [", paste(vec_itemcpos_id, collapse = " "), "] explain a statistically important part of score variance.")
  }
  
  if (!is.null(vec_itemcneg_id) & (length(vec_itemcneg_id) > 0)) {
    if (length(vec_itemcneg_id) == 1)
      results_text_itemcneg <- paste0("Item characteristic [", paste(vec_itemcneg_id, collapse = " "), "] explains inconsistency in scores within scale dimension.")
    else
      results_text_itemcneg <- paste0("Item characteristics [", paste(vec_itemcneg_id, collapse = " "), "] explain inconsistency in scores within scale dimension.")
  }
  
  return(list("results_text_scalepos" = results_text_scalepos, 
         "results_text_scaleneg" = results_text_scaleneg, 
         "results_text_itemcpos" = results_text_itemcpos, 
         "results_text_itemcneg" = results_text_itemcneg))
  
}