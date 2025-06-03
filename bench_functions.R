library(tidyverse)
library(benchmarkme)

r_bench <- function(runs = 3, write = TRUE){
  
  res_single <- benchmark_std(runs) # serial
  #upload_results(res)
  #plot(res)
  
  res_single %>% 
    group_by(test, test_group) %>%
    mutate(user = mean(user),
           system = mean(system),
           elapsed = mean(elapsed)) %>%
    unique() %>%
    mutate(type = 'agg') -> agg
  
  res_single$type = 'orig'
  res_single <- rbind(res_single, agg)
  res_single$cores <- 0
  
  res_par1 <- benchmark_std(runs, cores = 1) # parallel but only 1 core
  #upload_results(res)
  #plot(res)
  
  res_par1 %>% 
    group_by(test, test_group) %>%
    mutate(user = mean(user),
           system = mean(system),
           elapsed = mean(elapsed)) %>%
    unique() %>%
    mutate(type = 'agg') -> agg
  
  res_par1$type = 'orig'
  res_par1 <- rbind(res_par1, agg)
  res_par1$cores <- 1
  
  # multicore
  # get no of cores
  n_cores <- get_cpu()$no_of_cores
  
  res_multi <- benchmark_std(runs, cores = n_cores) # true parallel
  #upload_results(res)
  #plot(res_multi)
  
  res_multi %>% 
    group_by(test, test_group) %>%
    mutate(user = mean(user),
           system = mean(system),
           elapsed = mean(elapsed)) %>%
    unique() %>%
    mutate(type = 'agg') -> agg
  
  res_multi$type = 'orig'
  
  res_multi <- rbind(res_multi, agg)
  res_multi$cores <- n_cores
  
  res <- rbind(res_single, res_par1, res_multi)
  res$cpu <- get_cpu()$model_name
  res$ram <- round(get_ram() / 1024^3, 1)
  
  if(write){
    f_name <- paste0('Comp_', get_cpu()[2:3] %>%
                       paste0(., collapse = '_') %>%
                       gsub('@', '', .) %>% 
                       gsub(' ', '-', .), '_',
                     stringi::stri_rand_strings(1, length = 5),
                     '.csv')
    
    write_csv(res, f_name)
    
  }
  
  return(res)
  
}

io_bench <- function(runs = 3, write = TRUE){
  
  res_single <- benchmark_io(runs) # serial
  #upload_results(res)
  #plot(res)
  
  res_single %>% 
    group_by(test, test_group) %>%
    mutate(user = mean(user),
           system = mean(system),
           elapsed = mean(elapsed)) %>%
    unique() %>%
    mutate(type = 'agg') -> agg
  
  res_single$type = 'orig'
  res_single <- rbind(res_single, agg)
  res_single$cores <- 0
  
  res_par1 <- benchmark_io(runs, cores = 1) # parallel but only 1 core
  #upload_results(res)
  #plot(res)
  
  res_par1 %>% 
    group_by(test, test_group) %>%
    mutate(user = mean(user),
           system = mean(system),
           elapsed = mean(elapsed)) %>%
    unique() %>%
    mutate(type = 'agg') -> agg
  
  res_par1$type = 'orig'
  res_par1 <- rbind(res_par1, agg)
  res_par1$cores <- 1
  
  # multicore
  # get no of cores
  n_cores <- get_cpu()$no_of_cores
  
  res_multi <- benchmark_io(runs, cores = n_cores) # true parallel
  #upload_results(res)
  #plot(res_multi)
  
  res_multi %>% 
    group_by(test, test_group) %>%
    mutate(user = mean(user),
           system = mean(system),
           elapsed = mean(elapsed)) %>%
    unique() %>%
    mutate(type = 'agg') -> agg
  
  res_multi$type = 'orig'
  
  res_multi <- rbind(res_multi, agg)
  res_multi$cores <- n_cores
  
  res <- rbind(res_single, res_par1, res_multi)
  res$cpu <- get_cpu()$model_name
  res$ram <- round(get_ram() / 1024^3, 1)
  
  if(write){
    f_name <- paste0('IO_', get_cpu()[2:3] %>%
                       paste0(., collapse = '_') %>%
                       gsub('@', '', .) %>% 
                       gsub(' ', '-', .), '_',
                     stringi::stri_rand_strings(1, length = 5),
                     '.csv')
    
    write_csv(res, f_name)
    
  }
  
  return(res)
  
}

while(TRUE) {
  r_bench(write = FALSE)
  io_bench(write = FALSE)
  print(
    paste0('finished 3 iterations of both tests, ',
           stringi::stri_rand_strings(1, length = 5))
  )
}

