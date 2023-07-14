
# function used to compute the Wald stat for RDD results
compute_wald <- function(random_sample, tau, m) {
  
  rdd <- NA
  
  if (is.na(m)) {
    
    M_m_tau <- random_sample |>
               mutate(
                 Y = (as.numeric((X > -0.99 & X < 0)) * (0.48 + (1.27 * X) + (7.18 * X^2) + (20.21 * X^3) + (21.54 * X^4) + (7.33 * X^5) + U)) +
                     (as.numeric((X >= 0 & X <= 0.99)) * (0.48 + tau + (0.84 * X) - (3 * X^2) + (7.99 * X^3) - (9.01 * X^4) + (3.56 * X^5) + U))
               )
    
    rdd <- rdrobust(y = M_m_tau$Y, x = M_m_tau$X, c = 0)
    
  } else {
    
    M_m_tau <- random_sample |>
               mutate(
                 Y = (as.numeric((X > -0.99 & X < 0)) * (0.48 + (tau * plogis((4 * m * X) / tau)) + (1.27 * X) + (7.18 * X^2) + (20.21 * X^3) + (21.54 * X^4) + (7.33 * X^5) + U) +
                     (as.numeric((X >= 0 & X <= 0.99)) * (0.48 + (tau * plogis((4 * m * X) / tau)) + (0.84 * X) - (3 * X^2) + (7.99 * X^3) - (9.01 * X^4) + (3.56 * X^5) + U)))
               )
    
    rdd <- rdrobust(y = M_m_tau$Y, x = M_m_tau$X, c = 0)
    
  }
  
  return(abs(rdd$coef[3] / rdd$se[3]))
  
}