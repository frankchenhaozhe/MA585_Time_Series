


# get net flows and log returns and moving 
return_and_flow <- function(df){
  df %<>% dplyr::mutate(Asset = `Adj Close` * Volume,
                        net_flow = Asset - dplyr::lag(Asset),
                        log_return = log(`Adj Close`/lag(`Adj Close`)))
}

# add week, month, quater ticks
create_ticks <- function(df){
  df %<>% dplyr::mutate(year = year(Date),
                        month = month(Date),
                        week = week(Date),
                        weekdays = weekdays(Date))
  return(df)
}

# scale asset value and net_flow by million
scale_by_million <- function(df){
  df %<>% dplyr::mutate(Asset = Asset/1e6,
                        net_flow = net_flow/1e6)
}

# label outliers
label_outlier <- function(df, impute = T){
  ###------------------------------------
  ## METHOD:
  #
  # Use lowess to fit the curve and then use the residual
  # to check for the outlier.
  # Right now, the smoothing is using cloest 30 data points,
  # approximately 1.5 month. This smoothing factor is
  # achieved by empirical observations on the stock
  # Might not be optimal, but useful in this case
  #
  #
  # After getting the residuals, 'z' score is caculated
  # as the following:
  # 
  # 1. take absolute value of the residuals
  # 2. calculate the standard deviation of the residuals
  # 3. 'z' = abs(resid)/sd(abs(resid))
  # 4. pick 'z' greater than 3 as outliers
  # 5. label the outlier and get a new de-outlier series
  # 6. If choose to impute, the lowess value be used
  #    as the imputed value, otherwise it will be left 
  #    as NA 
  #
  ## Limitations:
  #
  # Becasue 30 days was used to smooth the NAV, there will be
  # issue when the series have high volatility at begining/end
  # 15 days period. 
  ###------------------------------------
  df$lowess <- lowess(df$net_flow ,
                      f = 30/nrow(df))$y # use 30 days
  if(impute){
    df %<>% dplyr::mutate(resid = abs(net_flow -lowess),
                          z = resid/sd(resid, na.rm = T),
                          de_outlier = dplyr::if_else(z < 3, 
                                                      net_flow,
                                                      lowess))
    
    df %<>% dplyr::mutate(de_outlier = if_else(is.na(de_outlier), 
                                               net_flow,
                                               de_outlier))
    
    return(df %>%
             dplyr::select(-resid, -z, -lowess))
  }
  
  df %<>% dplyr::mutate(resid = abs(`Adj Close`-lowess),
                        z = resid/sd(resid, na.rm = T),
                        de_outlier = dplyr::if_else(z < 3, 
                                                    net_flow,
                                                    NA))
  return(df %>%
           dplyr::select(-resid, -z, -lowess))
}


# adding moving average smoother and impute the missing values
add_smoothers_ma <- function(df){
  
  df %<>% dplyr::mutate(ma_5 = ma(net_flow, 5),
                        ma_21 = ma(net_flow, 21))
  df %<>% dplyr::mutate(ma_5 = if_else(is.na(ma_5), as.numeric(net_flow), as.numeric(ma_5)),
                        ma_21 = if_else(is.na(ma_21), as.numeric(net_flow), as.numeric(ma_21)))
}


# final cleaning
df_clean <- function(df){
  df <- df %>% 
    return_and_flow() %>% 
    create_ticks() %>%
    scale_by_million() %>% 
    label_outlier() %>% 
    add_smoothers_ma() %>%
    dplyr::select(-Open,-High,-Low,-Close,-Volume)
  
}

# slice the dataframe by date, can specify before or after the date
slice_by_date <- function(df, after = F, date = '2020-01-02'){
  
  if(after){
    return(df %>% filter(Date >= date))
  }
  return(df %>% filter(Date < date))
}


## reconcile daily asset from flows
reconcile_asset <- function(df, flow_name = 'net_flow'){
  # get capital gain rate
  df %<>% dplyr::mutate(cap_gain_rate = log(Asset - net_flow)/dplyr::lag(Asset))
  
  flow <- df[flow_name] %>% dplyr::pull()
  n = nrow(df)
  recon_asset <- rep(df$Asset[1], n)
  
  for (i in 2:n) {
    recon_asset[i] <- recon_asset[i-1]*exp(df$cap_gain_rate[i]) + flow[i]
  }
  df$recon_asset <- recon_asset 
  return(df)
}

## the evaluation metrics for model comparison
get_dates <- function(df, year = 2020){
  df_new <- df[lubridate::year(df$Date) == year,]
  return(df_new)
}


eval_asset <- function(df, year = 2020, flow_name ='de_outlier'){
  # this function assumes that column name 'de_outlier' 
  # is forecasted value
  # so the Asset reconciled by the forecasted flow
  res <- df %>% 
    get_dates(year) %>% 
    reconcile_asset(flow_name = flow_name)
  
  avg_true_asset <- mean(res$Asset)
  avg_fc_asset <- mean(res$recon_asset)
  
  return((avg_fc_asset - avg_true_asset)/avg_true_asset)
}


SNP500 <- SNP500_orig %>% df_clean()
test_orig <- SNP500 %>% slice_by_date(after = T)
test_orig <- test_orig$Date
train_orig <- SNP500 %>% slice_by_date()
train_date <- train_orig$Date



p1 <- ggplot(SNP500, aes(x = Date, y =`Adj Close`)) + 
  geom_line(aes(color = '`Adj Close`'), size = 0.5) + 
  scale_color_manual(values = c("#00AFBB")) +
  theme_minimal() + 
  labs(title = ' ', y = ' ', x = ' ')

sub1 <- SNP500 %>% 
  dplyr::select(Date, net_flow, de_outlier) %>%
  dplyr::mutate(net_flow = scale(net_flow) - 2 , 
                de_outlier = scale(de_outlier) + 2)
sub1 %<>% tidyr::pivot_longer(-Date, names_to = 'flow_vs_smoothed')

p2 <- ggplot(sub1, aes(x = Date, y = value)) + 
  geom_line(aes(color = flow_vs_smoothed), size = 0.4) +
  scale_color_manual(values = c("#00AFBB","#E7B800")) +
  theme_minimal() +
  labs(x = ' ')

sub2 <- SNP500 %>% 
  dplyr::select(Date, de_outlier, ma_5, ma_21)%>%
  dplyr::mutate(de_outlier = scale(de_outlier) , 
                ma_5 = scale(ma_5) - 2,
                ma_21 = scale(ma_21) + 2)
sub2 %<>% tidyr::pivot_longer(-Date, names_to = 'smoothed_flows')

p3 <- ggplot(sub2, aes(x = Date, y = value)) + 
  geom_line(aes(color = smoothed_flows), size = 0.4) +
  scale_color_manual(values = c("#00AFBB","red","blue")) +
  theme_minimal()


