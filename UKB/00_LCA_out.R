library(dplyr)
LCA_out <- function(LCA_list, n_class, n_var){
  #
  SES_LCA <- LCA_list[[n_class]]
  # prevelence
  Prev <- SES_LCA$P %>% round(.,2)
  # mean posterior probability
  mean_PIP <- vector(mode = "numeric", length = n_class)
  for (n in 1:n_class) {
    mean_PIP[n] <- SES_LCA$posterior[which(SES_LCA$predclass == n), n] %>% 
      mean %>% round(.,2)
  }
  # probabilities with different latent classes
  probs <- Reduce(cbind, SES_LCA$probs) %>% round(.,2)
  if (n_var == 3) {
    colnames(probs) <- c(paste0(names(SES_LCA$probs)[1], "_", 1: ncol(SES_LCA$probs[[1]])),
                         paste0(names(SES_LCA$probs)[2], "_", 1: ncol(SES_LCA$probs[[2]])),
                         paste0(names(SES_LCA$probs)[3], "_", 1: ncol(SES_LCA$probs[[3]])))
  } else if (n_var == 4) {
    colnames(probs) <- c(paste0(names(SES_LCA$probs)[1], "_", 1: ncol(SES_LCA$probs[[1]])),
                         paste0(names(SES_LCA$probs)[2], "_", 1: ncol(SES_LCA$probs[[2]])),
                         paste0(names(SES_LCA$probs)[3], "_", 1: ncol(SES_LCA$probs[[3]])), 
                         paste0(names(SES_LCA$probs)[4], "_", 1: ncol(SES_LCA$probs[[4]])))
  }

  
  # format output
  out_mat <- cbind(data.frame(mean_PIP = mean_PIP, Prev = Prev),
                   probs) %>% t()
  out_vector <- c(SES_LCA$aic, SES_LCA$bic, SES_LCA$Gsq)
  names(out_vector) <- c("AIC", "BIC", "G2")
  #
  out_list <- list(out_vector, out_mat)
  return(out_list)
  
}
