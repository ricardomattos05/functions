########### Binning analysis


bin_var <- function(df, y) {
  ## transform vars with low lenght in factor variable
  for (i in 1:length(df)) {
    if (!is.factor(df[, i]) & length(unique(df[, i])) <= 10) {
      df[, i] <- as.factor(df[, i])
    }
  }
  
  #Placing the y factor to the end of the table so it will not be valuated.
  only.y <- df[, y, drop = FALSE]
  only.fctrs <-
    df[,!(names(model_full_sample) %in% names(only.y))] %>% as.data.frame()
  
  df <- cbind(only.fctrs, only.y)
  
  #### OverSampling to get real efect
  flag1 <- df %>%
    filter(get(y) == 1)
  
  flag0 <- df %>%
    filter(get(y) == 0) %>%
    sample_n(nrow(flag1))
  
  df2 <- rbind(flag1, flag0)
  
  
  vars_bin <- as.data.frame(seq(1:nrow(df)))
  for (i in 1:(length(df2) - 1)) {
    if (length(unique(df2[, i])) <= 2) {
      vars_bin <- cbind(vars_bin, df[, i])
      names(vars_bin)[length(vars_bin)] = paste0(names(df)[i], ".binned")
    }
    ### When var is factor, tree.bins will be used
    else if (is.factor(df2[, i])) {
      bin <-
        tree.bins(
          df2[, c(i, length(df2))],
          bin.nm = paste0("bin_", names(df2)[i]),
          y = Response,
          return = "lkup.list"
        )
      
      aux1 <- as.data.frame(df[, i])
      colnames(aux1) <- names(df)[i]
      aux <-
        left_join(aux1, as.data.frame(bin), by = names(bin[[1]])[1])
      colnames(aux) <-
        c(names(bin[[1]])[1], paste0(names(bin[[1]])[1], ".binned"))
      vars_bin <- cbind(vars_bin, aux[, 2])
      names(vars_bin)[length(vars_bin)] = names(aux)[2]
    }
    else {
      ### When is numeric or integer, tree.bins will be used
      woe <-
        woe.binning(df2,  names(df2)[length(df2)],  names(df2)[i])
      bin <- woe.binning.deploy(df[, c(i, length(df))], woe)
      # colnames(bin) <- c(names(bin)[1],names(bin)[2],names(bin)[3])
      vars_bin <- cbind(vars_bin, bin[, 3])
      names(vars_bin)[length(vars_bin)] = names(bin)[3]
    }
  }
  
  vars_bin[, -1]
  
}
