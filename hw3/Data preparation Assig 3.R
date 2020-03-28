
if (!require("dplyr")) install.packages("dplyr")

# Leaving out the weights, future expectations, gender and country
df_sample <- df_sample %>% select(-c(ST004D01T,BSMJ,W_FSTUWT,CNT))
# Convert all variable types to numeric
df_sample <- as.data.frame(apply(df_sample, 2, as.numeric))
# Scaling
df_sample <- as.data.frame(apply(df_sample, 2,
                                 function(x) (x - min(x)) / (max(x) - min(x))))