library(tidyverse)

# load data and remove really weird locations (size <= 2 or size >= 10k)
nets_size_2013 <- read_rds('Data/CA_business_sizes_2013.rds') %>% 
  filter(between(Emp13, 3, 10000)) %>% 
  # just to make everything easier, let's just get a 10% sample of businesses
  sample_frac(0.1)

# some baseline stats ... average business size and size of business for average emp
nets_size_2013 %>% 
  summarise(tot_ests_pop = n(),
            tot_emps_pop = sum(Emp13),
            avg_size_estab = mean(Emp13),
            avg_size_emply = sum(Emp13^2)/sum(Emp13))

# a couple quick plots show that while most businesses are small, most people work for larger businesses
nets_size_2013 %>% 
  ggplot(aes(Emp13, y = ..density..)) + 
  geom_histogram() +
  scale_x_log10()

nets_size_2013 %>% 
  ggplot(aes(Emp13, y = ..density.., weight = Emp13)) + 
  geom_histogram() +
  scale_x_log10()

# first, what are our sampling assumptions?
# x1 ~ fully random / equal probability
# x2 ~ directly proportional to size


# Chao method uses only sample size, f1, and f2
nhat_chao <- function(n, f1, f2) {
  n + f1^2 / (2 * f2) 
}

# Zelterman method uses only sample size, f1, and f2
nhat_zelt <- function(n, f1, f2) {
  n / (1 - exp(-2 * f2 / f1))
}

# function needs to take a run id sample size, and weight rule
# return number of locations, mean size, truncated mean size, median size as tibble
# HT estimator SHOULD be added to this
sample_summary <- function(run, size, source_data = nets_size_2013, summary_col = Emp13, weight = NULL) {
  
  if (!exists('srun')) {
    cat(run, '\n')
    srun <<- run
  } else if (srun != run) {
    cat(run, '\n')
    srun <<- run
  } 
  
  # sample businesses and collapse to uniques
  s <- source_data %>% 
    sample_n(size, replace = TRUE, weight = {{ weight }}) %>% 
    group_by_all() %>% 
    count() %>% 
    ungroup()
  
  # get number of ones and twos for chao / zelt
  ones_and_twos <- s %>% 
    count(n) %>% 
    filter(n <= 2) %>% 
    deframe()
  
  # chao <- nhat_chao(nrow(s), ones_and_twos[1], ones_and_twos[2])
  # zelt <- nhat_zelt(nrow(s), ones_and_twos[1], ones_and_twos[2])
  
  # other summary stats to do with employee size
  s %>% 
    summarize(tot_ests_sample = n(),
              tot_emps_sample = sum({{ summary_col }}),
              mean_emps_sample = mean({{ summary_col }}),
              trmn_emps_sample = mean({{ summary_col }}, trim = 0.05),
              med_emps_sample = median({{ summary_col }})) %>% 
    mutate(run_no = run,
           sample_size = size,
           chao = nhat_chao(tot_ests_sample, ones_and_twos[1], ones_and_twos[2]),
           zelt = nhat_zelt(tot_ests_sample, ones_and_twos[1], ones_and_twos[2])) %>% 
    select(run_no, sample_size, everything())
}

# run 100 tests per sample size, every 5% of population
sample_results <- crossing(run = 1:100, 
                           size = sum(nets_size_2013$Emp13) * seq(0.005, 0.20, 0.005)) %>% 
  pmap_dfr(sample_summary, weight = Emp13)

# relative sample plots (2)
sample_results %>% 
  mutate(sample_frac = sample_size / sum(nets_size_2013$Emp13)) %>% 
  ggplot(aes(sample_frac, tot_ests_sample)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = nrow(nets_size_2013)) +
  geom_abline(slope = sum(nets_size_2013$Emp13)) +
  scale_x_continuous(labels = scales::percent_format(1)) +
  scale_y_continuous(labels = scales::comma) +
  expand_limits(x = 0, y = 0) +
  labs(x = 'Sample Fraction (people in sample / population)',
       y = 'Total Locations in Sample') +
  theme_bw()

sample_results %>% 
  mutate(sample_frac = sample_size / sum(nets_size_2013$Emp13)) %>% 
  ggplot(aes(sample_frac, tot_emps_sample)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = sum(nets_size_2013$Emp13)) +
  geom_abline(slope = sum(nets_size_2013$Emp13)) +
  scale_x_continuous(labels = scales::percent_format(1)) +
  scale_y_continuous(labels = scales::comma) +
  expand_limits(x = 0, y = 0) +
  labs(x = 'Sample Fraction (people in sample / population)',
       y = 'Total Employees of Locations in Sample') +
  theme_bw()

# mean size plot
sample_results %>% 
  mutate(sample_frac = sample_size / sum(nets_size_2013$Emp13)) %>% 
  select(sample_frac, mean_emps_sample:med_emps_sample) %>% 
  pivot_longer(-sample_frac) %>% 
  mutate(name = case_when(name == 'mean_emps_sample' ~ 'Mean employees',
                          name == 'trmn_emps_sample' ~ 'Trimmed (5%) Mean employees',
                          name == 'med_emps_sample'  ~ 'Median employees')) %>% 
  ggplot(aes(sample_frac, value, group = name, color = name)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = mean(nets_size_2013$Emp13)) +
  scale_x_continuous(labels = scales::percent_format(1)) +
  expand_limits(x = 0, y = 0) +
  labs(x = 'Sample Fraction (people in sample / population)',
       y = 'Estimated mean location size') +
  theme_bw()

# chao and zelterman plots
sample_results %>% 
  mutate(sample_frac = sample_size / sum(nets_size_2013$Emp13)) %>% 
  select(sample_frac, chao, zelt, tot_ests_sample) %>% 
  pivot_longer(-sample_frac) %>% 
  mutate(name = case_when(name == 'chao'            ~ 'Nhat (Chao)',
                          name == 'zelt'            ~ 'Nhat (Zelterman)',
                          name == 'tot_ests_sample' ~ 'n (locations in sample)')) %>% 
  ggplot(aes(sample_frac, value, group = name, color = name)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = nrow(nets_size_2013)) +
  scale_x_continuous(labels = scales::percent_format(1)) +
  scale_y_continuous(labels = scales::comma) +
  expand_limits(x = 0, y = 0) +
  labs(x = 'Sample Fraction (people in sample / population)',
       y = 'Estimated total number of locations') +
  theme_bw()

# takeaways: 
# + employees (proxy for chargers) gets pretty good around 5% sample
# + maybe interesting story since it IDs failure locs for chao and zelterman
# - our sample is for sure missing locations
# - assuming ~ similar distribution, a 10% sample would miss ~ half of locations

