#theme setting
dio_theme <-
  theme(panel.background = element_rect(fill = "#f5f5f5"),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0),
        legend.position="bottom")

##simulate for growth case per double, triple, quadruple, septuple 
between_fun <- function(first_value, second_value, multiplier){
  value <- (second_value - first_value) /multiplier
  result <- c()
  for (i in 1:multiplier-1) {
    between_value <- first_value + ( value * i)
    result[i] <- between_value
  }
  return(result)
}

seq_fun <- function(start_value, end_value, multiplier){
  rst <- c()
  val <- start_value
  while (val <= end_value) {
    #print(val)
    rst <- append(rst,val)
    next_val <- val * 2
    in_between <- between_fun(val , next_val, multiplier)
    in_between <- in_between[in_between < end_value]
    rst <- append(rst,in_between)
    val <- next_val
  }
  return(rst)
}
simulation_df <- data.frame()
n <- c(2,3,4,7)
for (i in n) {
  sim <- seq_fun(start_value=100, end_value = 1000000,i)
  day_delay = paste('per:',i,'days')
  n = length(sim)
  df <- data.frame(val=sim, double_each = day_delay,seq_=seq(0,n-1))
  simulation_df <- rbind(df,simulation_df)
}


#subset data only for top 5 country based on #of cases
top5_list<- function(set){
  set %>%
    filter(!country %in% c('China','United Kingdom')) %>% #excluding due to data issue
    group_by(country) %>%
    summarise(ncases= max(cases)) %>%
    arrange(desc(ncases)) %>%
    head(5)
}

#template for plotting number of cases 
cumulative_case_plot <- function(set,legend){
  ggplot()+
    geom_line(data=simulation_df,
              aes(x=seq_,
                  y=val,
                  group= double_each),
              linetype="dashed") +
    labs(x = "#of days since 100th case",
         y = "#of Confirmed Cases",
         title = paste("Cumulative Confirmed Case Per", Sys.Date()),
         subtitle = "Simulation Testing: Start on first 100 cases and Stop before achieving 1 million cases",
         caption = "modified from:\n@dioariadi | https://datawizart.com/\n@jburnmurdoch (Financial Times) | https://www.ft.com/coronavirus-latest\nData From: https://github.com/CSSEGISandData")+
    scale_y_log10(labels = scales::comma)+
    scale_x_continuous(breaks = c(seq(0,350,7)))+
    annotate(geom = "text", x = 18, y = 800000, label = "Case Double\nEvery 2 days", color = "black", angle = 0,size =3)+
    annotate(geom = "text", x = 32, y = 800000, label = "Case Double\nEvery 3 days", color = "black", angle = 0,size =3)+
    annotate(geom = "text", x = 49, y = 800000, label = "Case Double\nEvery 4 days", color = "black", angle = 0,size =3)+ 
    annotate(geom = "text", x = 88, y = 140000, label = "Case Double\nEvery week", color = "black",angle = 0,size =3) +
    geom_line(aes(x=seq_-1, y=cases, group=country, colour=category), data = set,show.legend = legend)+ #country with highest cases
    geom_text_repel(aes(x=seq_-1,y=cases,label=country),
                    nudge_x = 1,
                    na.rm = TRUE,
                    segment.size =0.05,
                    data = set %>%dplyr::filter(cases==max(cases))) +
    dio_theme
}

#Append Indonesia data and top 5 cities
top5_province  <-
  function(set){
    set %>%
      group_by(province) %>%
      summarise(ncases= max(total_case)) %>%
      arrange(desc(ncases)) %>%
      head(7)
  }

append_data <- function(global_data, indo_data){
  global_data %>%
    dplyr::filter(country=='Indonesia') %>%
    bind_rows(
      indo_data %>%
        dplyr::select(province,data_date, total_case) %>%
        rename(country = province,
               data_date =data_date,
               cases=total_case) %>%
        dplyr::filter(country %in% (unique(province_list$province)))
    )
}


# Compute new cases and smooth them
smooth_new_cases <- function(cases){
  cases %>%
    group_by(country) %>%
    arrange(data_date) %>%
    mutate(new_cases = c(cases[1], diff(cases))) %>%
    mutate(new_cases_smooth = round(
      smoother::smth(new_cases, window = 7, tails = TRUE)
    )) %>%
    dplyr::select(country, data_date, new_cases, new_cases_smooth) %>%
    ungroup()
}

convert_isoweek <- function(cases){
  cases %>%
    arrange(data_date) %>%
    mutate(isoweek = cut(data_date,"week")) %>%
    group_by(isoweek,province) %>%
    summarise(weekly_case = sum(daily_case)) %>%
    ungroup()
}

# raw %>%
#   select(province,data_date,daily_case) %>%
#   mutate(isoweek = cut(data_date,'week')) %>%
#   left_join(convert_isoweek(raw))%>%
#   View()

plot_new_cases <- function(cases){
  cases %>%
    ggplot(aes(x = data_date, y = new_cases)) +
    geom_line(linetype = 'dotted', color = 'gray40') +
    geom_line(aes(y = new_cases_smooth), color = "#14243e") +
    labs(
      title = "New cases per day",
      subtitle = unique(cases$country),
      x = NULL, y = NULL
    )+
    scale_y_continuous(labels = scales::comma)+
    dio_theme
}


plot_new_cases_group <- function(cases){
  cases %>%
    ggplot(aes(x = data_date, y = new_cases)) +
    geom_line(linetype = 'dotted', color = 'gray40') +
    geom_line(aes(y = new_cases_smooth), color = "#14243e") +
    labs(
      title = "New cases per day",
      subtitle = unique(cases$country),
      x = NULL, y = NULL
    )+
    scale_y_continuous(labels = scales::comma)+
    dio_theme +
    facet_wrap(~country,strip.position = "right")
}

compute_likelihood <- function(cases){
  likelihood <- cases %>%
    dplyr::filter(new_cases_smooth > 0) %>%
    mutate(
      r_t = list(r_t_range),
      lambda = map(lag(new_cases_smooth, 1), ~ .x * exp(GAMMA * (r_t_range - 1))),
      likelihood_r_t = map2(new_cases_smooth, lambda, dpois, log = TRUE)
    ) %>%
    slice(-1) %>%
    dplyr::select(-lambda) %>%
    unnest(c(likelihood_r_t, r_t))
}

compute_posterior <- function(likelihood){
  likelihood %>%
    arrange(data_date) %>%
    group_by(r_t) %>%
    mutate(posterior = exp(
      zoo::rollapplyr(likelihood_r_t, 7, sum, partial = TRUE)
    )) %>%
    group_by(data_date) %>%
    mutate(posterior = posterior / sum(posterior, na.rm = TRUE)) %>%
    # HACK: NaNs in the posterior create issues later on. So we remove them.
    mutate(posterior = ifelse(is.nan(posterior), 0, posterior)) %>%
    ungroup() %>%
    dplyr::select(-likelihood_r_t)
}

estimate_rt <- function(posteriors){
  posteriors %>%
    group_by(country, data_date) %>%
    summarize(
      r_t_simulated = list(sample(r_t_range, 10000, replace = TRUE, prob = posterior)),
      r_t_most_likely = r_t_range[which.max(posterior)]
    ) %>%
    mutate(
      r_t_lo = map_dbl(r_t_simulated, ~ hdi(.x)[1]),
      r_t_hi = map_dbl(r_t_simulated, ~ hdi(.x)[2])
    ) %>%
    dplyr::select(-r_t_simulated)
}

plot_estimates <- function(estimates){
  
  estimates %>%
    ggplot(aes(x = data_date, y = r_t_most_likely)) +
    geom_point(color = "darkred", alpha = 0.8, size = 1) +
    geom_line(color = "#14243e") +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_ribbon(
      aes(ymin = r_t_lo, ymax = r_t_hi),
      fill = 'darkred',
      alpha = 0.2
    ) +
    labs(
      title = expression('Reproduction Number R'[t]), x = '', y = '',
      subtitle = unique(estimates$country)
    ) +
    dio_theme
}

