ProbPlotCalculateValues = function(data_obs, probs = NULL, PP = NULL, dist = NULL, T_rp = NULL, beta_CL = NULL,
                                   T_lim = NULL, Q_lim = NULL, main_title = NULL, x_lab = NULL, y_lab = NULL,
                                   Pcol = 'black', Ppch = 1, Pcex = 1, Lcol = 'blue', Lty = 1, Lwd = 1.5,
                                   CPlot = TRUE, CLcol = 'red', CLty = 2, CLwd = 1.5,
                                   QTcol = 'green', QTpch = 15, QTcex = 1.5, GumbRV = FALSE, P3SkewCheck = TRUE){
  
  ##############################################################################
  if(!is.vector(data_obs)){
    n = dim(data_obs)[1]
    x = data_obs[1:n, 1]}else{
      n = length(data_obs)
      x = data_obs[1:n]}
  
  x_bar = mean(x)
  s_x = sd(x)
  y = log(x)
  y_bar = mean(y)
  s_y = sd(y)
  y10 = log10(x)
  y10_bar = mean(y10)
  s_y10 = sd(y10)
  
  if(is.null(T_rp)){T_r = 5}else{T_r = T_rp}
  n_T = length(T_r)
  
  p_vec = sort(c(0.0001, 0.001, 0.01, 0.05, seq(0.1, 0.9, 0.1), 0.95, 0.99, 0.999, 0.9999))
  
  squants = sort(x)
  lsquants = log(squants)
  
  if(is.null(x_lab)){x_lab = expression(paste(F(x) == P(X <= x),' (%)'))}
  if(is.null(y_lab)){y_lab = 'Quantile'} 
  
  if(is.null(PP)){PP = 'Weibull'} 
  
  if(is.null(probs)){
    probs = PlotPos(data_obs = x, PP = PP)
  }
  
  if(is.null(beta_CL)){
    beta_CL = 0.95}
  
  if(is.null(dist)){
    dist = 'LPea3'}
  
  ##############################################################################
  if(dist == 'LPea3'){
    
    par(mar = c(5, 5, 8, 1))
    
    if(n < 30){
      C_sy = (n * sum((y - y_bar)^3))/((n - 1) * (n - 2) * s_y^3)}else{
        C_sy = sum((y - y_bar)^3)/(n * s_y^3)}
    
    alpha = 4/(C_sy^2)
    beta = (s_y * C_sy) / 2
    tau =  sign(C_sy) * (y_bar - 2 * (s_y / C_sy))
    
    FYy = rep(0, n)
    qFYy = rep(0, n)
    for(i in 1:n){FYy[i] = cdflP3(lsquants[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    qFYy[i] = qualP3(P = FYy[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    }
    
    q = rep(0, length(p_vec))
    for(i in 1:length(p_vec)){
      q[i] = qualP3(P = p_vec[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    }
    
    yq = rep(0, n)
    for(i in 1:n){
      yq[i] = qualP3(P = probs[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    }
    
    pT = 1 - 1/T_r
    qTlP3 = rep(0, n_T)
    for(i in 1:n_T){
      qTlP3[i] = qualP3(P = pT[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    }
    
    if(is.null(T_rp)){
      QuantsB = yq
      z_p = qnorm(probs)}else{
        QuantsB = sort(c(yq, qTlP3))
        z_p = qnorm(sort(c(probs, pT)))
      }
    
    K_p_prime = (1/6) * (z_p^2 - 1) + (1/9) * (z_p^3 - 6 * z_p) * C_sy/6 - (1/2) * (z_p^2 - 1) * (C_sy/6)^2 + (2 * z_p / 3) * (C_sy/6)^3 - (5/18) * (C_sy/6)^4
    K_p = (2/C_sy) * ((z_p - C_sy/6) * (C_sy/6) + 1)^3 - (2/C_sy)
    delta2 = 1 + C_sy * K_p + (1/2) * (1 + (3/4) * C_sy^2) * K_p^2 + 6 * (1 + (1/4) * C_sy^2) * K_p_prime * ((1/2) * C_sy * K_p + (1 + (5/4) * C_sy^2) * K_p_prime)
    delta = sqrt(delta2)
    S_e = s_y * delta / sqrt(n)
    
    Bounds = ConfInt(quant = QuantsB, n_samp = n, SE = S_e, ConfLev = beta_CL)
    
    mn = min(yq, qTlP3)
    MX = max(yq, qTlP3)
    
    if(is.null(T_lim)){
      x_lim = c(mn, MX)}else{
        qTlP3_lim =   rep(0, 2)
        qTlP3_lim[1] = qualP3(P = 1 - 1/T_lim[1], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
        qTlP3_lim[2] = qualP3(P = 1 - 1/T_lim[2], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
        if((min(T_lim) >= min(round(1/(1 - p_vec), digits = 2))) & (max(T_lim) <= max(round(1/(1 - p_vec), digits = 2)))){
          x_lim = c(min(qTlP3_lim), max(qTlP3_lim))}else{
            x_lim = c(mn, MX)
            warning('Inappropriate return period bounds')}
      }
    
    if(is.null(Q_lim)){
      y_lim = c(min(x, exp(yq), exp(qTlP3)), max(x, exp(yq), exp(qTlP3)))}else{
        y_lim = c(min(Q_lim), max(Q_lim))
      }
    
    # plot(x = NA, y = NA, xaxt = 'n',
    #      xlab = x_lab, ylab = y_lab,
    #      xlim = x_lim,
    #      ylim = y_lim, log = 'y')
    # 
    # if(is.null(main_title)){main_title = 'Log-Pearson type III Probability Plot'}
    # 
    # title(main = main_title, line = 6)
    # axis(1, at = q, labels = p_vec * 100)
    # axis(3, at = q, labels = round(1/(1 - p_vec), digits = 2))
    # mtext('Return Period (years)', side = 3, line = 3)
    # 
    # grid(nx = NA, ny = NULL) 
    # for(i in 1:length(p_vec)){
    #   abline(v = q[i], untf = FALSE, lty = 3)}
    # 
    # points(yq, squants, pch = Ppch, col = Pcol, cex = Pcex)
    # 
    # if(!is.null(T_rp)){
    #   points(qTlP3, exp(qTlP3), pch = QTpch, col = QTcol, cex = QTcex)
    #   segments(x0 = -10^6, y0 = exp(qTlP3), x1 = qTlP3, y1 = exp(qTlP3), col = QTcol)
    # }
    # 
    # LX = sort(c(yq, qTlP3))#qFYy
    # LY = sort(c(exp(yq), exp(qTlP3)))
    # 
    # lines(LX, LY, col = Lcol, lty = Lty, lwd = Lwd)
    # 
    # if(CPlot == TRUE){
    #   lines(QuantsB, exp(Bounds[ , 1]), col = CLcol, lty = CLty, lwd = CLwd)
    #   lines(QuantsB, exp(Bounds[ , 2]), col = CLcol, lty = CLty, lwd = CLwd)
    # }
    # 
    # legend('bottomright', legend = c('Observed Data', 'Theoretical Distribution', paste('Confidence Limits (', beta_CL * 100, '%)', sep = ''),
    #                                  ifelse(!is.null(T_rp), 'Quantile Estimate', NA)),
    #        col = c(Pcol, Lcol, CLcol, ifelse(!is.null(T_rp), QTcol, NA)),
    #        lty = c(0, Lty, CLty, ifelse(!is.null(T_rp), NA, NA)),
    #        pch =c(Ppch, NA, NA, ifelse(!is.null(T_rp), QTpch, NA)),
    #        bg = 'white')
    
    KStest = ks.test(lsquants, yq)
    
    if(!is.null(T_rp)){Q_T = exp(qTlP3); names(Q_T) = T_rp}
    
    return(list(GOF = KStest, if(!is.null(T_rp)){Q_Tr = Q_T}))}
}

###################################################################
ProbPlotOutputPlot = function(data_obs, probs = NULL, PP = NULL, dist = NULL, T_rp = NULL, beta_CL = NULL,
                              T_lim = NULL, Q_lim = NULL, main_title = NULL, x_lab = NULL, y_lab = NULL,
                              Pcol = 'black', Ppch = 1, Pcex = 1, Lcol = 'blue', Lty = 1, Lwd = 1.5,
                              CPlot = TRUE, CLcol = 'red', CLty = 2, CLwd = 1.5,
                              QTcol = 'green', QTpch = 15, QTcex = 1.5, GumbRV = FALSE, P3SkewCheck = TRUE){
  
  if(!is.vector(data_obs)){
    n = dim(data_obs)[1]
    x = data_obs[1:n, 1]}else{
      n = length(data_obs)
      x = data_obs[1:n]}
  
  x_bar = mean(x)
  s_x = sd(x)
  y = log(x)
  y_bar = mean(y)
  s_y = sd(y)
  y10 = log10(x)
  y10_bar = mean(y10)
  s_y10 = sd(y10)
  
  if(is.null(T_rp)){T_r = 5}else{T_r = T_rp}
  n_T = length(T_r)
  
  p_vec = sort(c(0.0001, 0.001, 0.01, 0.05, seq(0.1, 0.9, 0.1), 0.95, 0.99, 0.999, 0.9999))
  
  squants = sort(x)
  lsquants = log(squants)
  
  if(is.null(x_lab)){x_lab = expression(paste(F(x) == P(X <= x),' (%)'))}
  if(is.null(y_lab)){y_lab = 'Quantile'}
  
  if(is.null(PP)){PP = 'Weibull'}
  
  if(is.null(probs)){
    probs = PlotPos(data_obs = x, PP = PP)
  }
  
  if(is.null(beta_CL)){
    beta_CL = 0.95}
  
  if(is.null(dist)){
    dist = 'LPea3'}
  
  ##############################################################################
  if(dist == 'LPea3'){
    
    par(mar = c(5, 5, 8, 1))
    
    if(n < 30){
      C_sy = (n * sum((y - y_bar)^3))/((n - 1) * (n - 2) * s_y^3)}else{
        C_sy = sum((y - y_bar)^3)/(n * s_y^3)}
    
    alpha = 4/(C_sy^2)
    beta = (s_y * C_sy) / 2
    tau =  sign(C_sy) * (y_bar - 2 * (s_y / C_sy))
    
    FYy = rep(0, n)
    qFYy = rep(0, n)
    for(i in 1:n){FYy[i] = cdflP3(lsquants[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    qFYy[i] = qualP3(P = FYy[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    }
    
    q = rep(0, length(p_vec))
    for(i in 1:length(p_vec)){
      q[i] = qualP3(P = p_vec[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    }
    
    yq = rep(0, n)
    for(i in 1:n){
      yq[i] = qualP3(P = probs[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    }
    
    pT = 1 - 1/T_r
    qTlP3 = rep(0, n_T)
    for(i in 1:n_T){
      qTlP3[i] = qualP3(P = pT[i], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
    }
    
    if(is.null(T_rp)){
      QuantsB = yq
      z_p = qnorm(probs)}else{
        QuantsB = sort(c(yq, qTlP3))
        z_p = qnorm(sort(c(probs, pT)))
      }
    
    K_p_prime = (1/6) * (z_p^2 - 1) + (1/9) * (z_p^3 - 6 * z_p) * C_sy/6 - (1/2) * (z_p^2 - 1) * (C_sy/6)^2 + (2 * z_p / 3) * (C_sy/6)^3 - (5/18) * (C_sy/6)^4
    K_p = (2/C_sy) * ((z_p - C_sy/6) * (C_sy/6) + 1)^3 - (2/C_sy)
    delta2 = 1 + C_sy * K_p + (1/2) * (1 + (3/4) * C_sy^2) * K_p^2 + 6 * (1 + (1/4) * C_sy^2) * K_p_prime * ((1/2) * C_sy * K_p + (1 + (5/4) * C_sy^2) * K_p_prime)
    delta = sqrt(delta2)
    S_e = s_y * delta / sqrt(n)
    
    Bounds = ConfInt(quant = QuantsB, n_samp = n, SE = S_e, ConfLev = beta_CL)
    
    mn = min(yq, qTlP3)
    MX = max(yq, qTlP3)
    
    if(is.null(T_lim)){
      x_lim = c(mn, MX)}else{
        qTlP3_lim =   rep(0, 2)
        qTlP3_lim[1] = qualP3(P = 1 - 1/T_lim[1], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
        qTlP3_lim[2] = qualP3(P = 1 - 1/T_lim[2], mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy)
        if((min(T_lim) >= min(round(1/(1 - p_vec), digits = 2))) & (max(T_lim) <= max(round(1/(1 - p_vec), digits = 2)))){
          x_lim = c(min(qTlP3_lim), max(qTlP3_lim))}else{
            x_lim = c(mn, MX)
            warning('Inappropriate return period bounds')}
      }
    
    if(is.null(Q_lim)){
      y_lim = c(min(x, exp(yq), exp(qTlP3)), max(x, exp(yq), exp(qTlP3)))}else{
        y_lim = c(min(Q_lim), max(Q_lim))
      }
    
    # plot(x = NA, y = NA, xaxt = 'n',
    #      xlab = x_lab, ylab = y_lab,
    #      xlim = x_lim,
    #      ylim = y_lim, log = 'y')
    #
    # if(is.null(main_title)){main_title = 'Log-Pearson type III Probability Plot'}
    #
    # title(main = main_title, line = 6)
    # axis(1, at = q, labels = p_vec * 100)
    # axis(3, at = q, labels = round(1/(1 - p_vec), digits = 2))
    # mtext('Return Period (years)', side = 3, line = 3)
    #
    # grid(nx = NA, ny = NULL)
    # for(i in 1:length(p_vec)){
    #   abline(v = q[i], untf = FALSE, lty = 3)}
    #
    # points(yq, squants, pch = Ppch, col = Pcol, cex = Pcex)
    #
    # if(!is.null(T_rp)){
    #   points(qTlP3, exp(qTlP3), pch = QTpch, col = QTcol, cex = QTcex)
    #   segments(x0 = -10^6, y0 = exp(qTlP3), x1 = qTlP3, y1 = exp(qTlP3), col = QTcol)
    # }
    
    # LX = sort(c(yq, qTlP3))#qFYy
    # LY = sort(c(exp(yq), exp(qTlP3)))
    
    # lines(LX, LY, col = Lcol, lty = Lty, lwd = Lwd)
    #
    # if(CPlot == TRUE){
    #   lines(QuantsB, exp(Bounds[ , 1]), col = CLcol, lty = CLty, lwd = CLwd)
    #   lines(QuantsB, exp(Bounds[ , 2]), col = CLcol, lty = CLty, lwd = CLwd)
    # }
    #
    # legend('bottomright', legend = c('Observed Data', 'Theoretical Distribution', paste('Confidence Limits (', beta_CL * 100, '%)', sep = ''),
    #                                  ifelse(!is.null(T_rp), 'Quantile Estimate', NA)),
    #        col = c(Pcol, Lcol, CLcol, ifelse(!is.null(T_rp), QTcol, NA)),
    #        lty = c(0, Lty, CLty, ifelse(!is.null(T_rp), NA, NA)),
    #        pch =c(Ppch, NA, NA, ifelse(!is.null(T_rp), QTpch, NA)),
    #        bg = 'white')
    
    
    LX = sort(c(yq, qTlP3))#qFYy
    LY = sort(c(exp(yq), exp(qTlP3)))
    
    KStest = ks.test(lsquants, yq)
    
    if(!is.null(T_rp)){Q_T = exp(qTlP3); names(Q_T) = T_rp}
    
    savedvalues<- list(GOF = KStest, if(!is.null(T_rp)){Q_Tr = Q_T})
    
    names(savedvalues) <- c("GOF", "FloodEstimates")
    
    flood25 <- round(as.numeric(savedvalues$FloodEstimates[1]), digits = 0)
    flood50 <- round(as.numeric(savedvalues$FloodEstimates[2]), digits = 0)
    flood100 <- round(as.numeric(savedvalues$FloodEstimates[3]), digits = 0)
    
    floodreturnperiods <- c(25,50,100)
    resultingfloods <- c(flood25,flood50,flood100)
    
    returnperiods = 1/(1-probs)
    
    CLlower <- exp(Bounds[ , 1])
    CLupper <- exp(Bounds[ , 2])
    
    #Lengths may differ for the calculated values and observed values, this equates them
    df1 <- data.frame(yq,returnperiods,squants)
    names(df1) <- data.frame("yq","ReturnPeriod","ObservedFlow")
    
    df2 <- data.frame(QuantsB, LY, CLlower, CLupper)
    names(df2) <- c("yq", "EstimatedValue","LowerCI","UpperCI")
    
    df1_unique <- df1[!duplicated(df1$yq), ]
    df2_unique <- df2[!duplicated(df2$yq), ]
    
    merged_df <- merge(df2_unique, df1_unique, by = "yq", all.x = TRUE)
    
    last_value <- tail(merged_df$ReturnPeriod, 1)
    if (is.na(last_value)) {
      merged_df$ReturnPeriod[length(merged_df$ReturnPeriod)] <- 100.00
    }
    
    savedggplot <- {ggplot() +
        #Actual measured flood points
        geom_point(aes(x = merged_df$ReturnPeriod, y = merged_df$ObservedFlow), shape = 1) +
        
        #Estimated 25, 50, 100 year floods
        geom_point(aes(x = floodreturnperiods, y = resultingfloods), shape = 2, color = "blue", size = 3) +
        
        #Upper and lower confidence intervals
        geom_line(aes(merged_df$ReturnPeriod, y = merged_df$LowerCI), color = "red") +
        geom_line(aes(merged_df$ReturnPeriod, y = merged_df$UpperCI), color = "red") +
        
        #Fitted Log-Pearson Line
        geom_line(aes(merged_df$ReturnPeriod, y = merged_df$EstimatedValue)) +
        
        #Displaying estimated 25, 50, and 100 year floods
        geom_hline(yintercept = flood25, linetype = "dashed", color = "green", alpha = 0.35, size = 1) +
        geom_hline(yintercept = flood50, linetype = "dashed", color = "green", alpha = 0.35, size = 1) +
        geom_hline(yintercept = flood100, linetype = "dashed", color = "green", alpha = 0.35, size = 1.1) +
        
        #Graphical options
        labs(x = "Return Period (Years)", y = "Flow (cfs)") +
        scale_x_log10()
    }
    
    
    return(savedggplot)
  }
}

###################################################################

pdflP3 = function(yp3, alpha, beta, tau){
  expon = ifelse((((yp3 - tau)/beta) < 0) & (((alpha - 1) %% 1) != 0), round(alpha - 1), alpha - 1)
  density = (1/(abs(beta) * gamma(alpha))) * ((yp3 - tau)/beta)^expon * exp(-(yp3 - tau)/beta)
  return(density)
}

#############################################################
#' @importFrom stats pnorm pgamma

cdflP3 = function(yp3, mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy){
  if(yp3 <= 0){PlP3 = 0; return(PlP3)}
  if(sigma_p <= 0){stop('invalid parameters')}
  if(gamma_p <= 1e-06){PlP3 = pnorm(yp3, mu_p, sigma_p); return(PlP3)}else{
    alpha = 4/gamma_p^2
    zlP3 = 2 * (yp3 - mu_p)/(sigma_p * gamma_p) + alpha
    PlP3 = pgamma(pmax(0, zlP3), alpha)
    if(gamma_p < 0){PlP3 = 1 - PlP3}
    return(PlP3)
  }
}

#############################################################

lP3P = function(yp3, P){cdflP3(yp3) - P}

#############################################################

#' @importFrom stats qnorm qgamma

qualP3 = function(P, mu_p = y_bar, sigma_p = s_y, gamma_p = C_sy){
  if(P < 0){stop('invalid probability')}
  if(sigma_p <= 0){stop('invalid parameters')}
  if(abs(gamma_p) <= 1e-08){QlP3 = qnorm(P, mu_p, sigma_p); return(QlP3)}else{
    alpha = 4/gamma_p^2
    beta = abs(0.5 * sigma_p * gamma_p)
    QlP3 = ifelse(gamma_p > 0,  mu_p - alpha * beta + beta * pmax(0, qgamma(P, alpha)),
                  mu_p + alpha * beta - beta * pmax(0, qgamma(1 - P, alpha)))
    return(QlP3)}
}


###########################################################################################################

ConfInt = function(quant, n_samp, SE, ConfLev){
  
  if(n_samp <= 30){
    L_Talpha = quant - qt(p = 1 - (1 - ConfLev)/2, df = n_samp - 1) * SE
    U_Talpha = quant + qt(p = 1 - (1 - ConfLev)/2, df = n_samp - 1) * SE}else{
      
      L_Talpha = quant - qnorm(p = 1 - (1 - ConfLev)/2) * SE
      U_Talpha = quant + qnorm(p = 1 - (1 - ConfLev)/2) * SE
    }
  
  return(ConfLim = data.frame(Lower_Limit = L_Talpha, Upper_Limit = U_Talpha))
}

###########################################################################################################