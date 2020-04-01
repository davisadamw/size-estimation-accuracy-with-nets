library(tidyverse)

nets_file <- '~/Side-Projects/NETS/extract-sb-from-nets/Raw_Data/NETS2013_CA(with Address).txt'

nets_size_2013 <- vroom::vroom(nets_file, col_select = one_of('DunsNumber', 'Emp13'), altrep = FALSE)

# get rid of the specification attribute, since it adds nothing
attr(nets_size_2013, 'spec') <- NULL

nets_size_2013 %>% 
  filter(!is.na(Emp13)) %>%
  write_rds('Data/CA_business_sizes_2013.rds', compress = 'gz')

