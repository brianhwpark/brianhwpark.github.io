# HOUSEKEEPING
user <- "C:/Users/brian/"
dir1 <- "Dropbox/Teaching/Concordia College/"
proj <- "[BUSN320] Business Statistics/BUSN320-Slides/figures"
setwd(file.path(user, dir1, proj))

# Population of 20 FAR–ORD flight times
flight_times <- c(
  104, 112, 97, 121, 108,
  115, 101, 126, 110, 106,
  99, 118, 103, 124, 107,
  113, 95, 129, 105, 107
)

# Generate Every Possible Sample of Size 10 without Replacement
samples <- combn(flight_times,10)

# Calculate the Mean of Every Possible Sample
sample_means <- colMeans(samples)

# Construct the Sampling Distribution
mean_distribution <- as.data.frame(table(sample_means))
names(mean_distribution) <- c("Sample_Mean", "Frequency")
mean_distribution$Sample_Mean <-
  as.numeric(as.character(mean_distribution$Sample_Mean))
mean_distribution$Probability <-
  mean_distribution$Frequency / sum(mean_distribution$Frequency)

# Plot the Sampling Distribution of the Sample Mean
png("busn-320-ch07-panel01-fig01.png",width=700,height=700)
plot(
  mean_distribution$Sample_Mean,
  mean_distribution$Probability,
  type = "h",
  lwd = 3,
  xlab = "Sample Mean Flight Time",
  ylab = "Probability",
  main = "Sampling Distribution of the Sample Mean"
)
dev.off()



#install.packages("ggplot2")
#library(ggplot2)

set.seed(320)

# All four populations have:
# Population mean = 100
# Population standard deviation = 20

population_mean <- 100
population_sd   <- 20

# ------------------------------------------------------------
# 1. Define functions for drawing from each population
# ------------------------------------------------------------

draw_population <- function(distribution, n) {
  if (distribution == "Normal") {
    values <- rnorm(
      n,
      mean = population_mean,
      sd = population_sd
    )
  } else if (distribution == "Uniform") {
    lower_bound <- population_mean - population_sd * sqrt(3)
    upper_bound <- population_mean + population_sd * sqrt(3)
    values <- runif(
      n,
      min = lower_bound,
      max = upper_bound
    )
  } else if (distribution == "Bimodal") {
    # Component means are chosen so the overall
    # population mean is 100 and SD is 20.
    component_sd <- 5
    distance_from_mean <- sqrt(
      population_sd^2 - component_sd^2
    )
    group <- sample(
      c(-1, 1),
      size = n,
      replace = TRUE
    )
    values <- rnorm(
      n,
      mean = population_mean +
        group * distance_from_mean,
      sd = component_sd
    )
  } else if (distribution == "Exponential") {
    # An exponential distribution with mean and SD equal to 20,
    # shifted right by 80.
    values <- 80 + rexp(
      n,
      rate = 1 / 20
    )
  }
  return(values)
}

# ------------------------------------------------------------
# 2. Display the four population distributions
# ------------------------------------------------------------

distributions <- c(
  "Normal",
  "Uniform",
  "Bimodal",
  "Exponential"
)
population_data <- data.frame()
for (distribution_name in distributions) {
  
  simulated_values <- draw_population(
    distribution = distribution_name,
    n = 100000
  )
  temporary_data <- data.frame(
    value = simulated_values,
    distribution = distribution_name
  )
  population_data <- rbind(
    population_data,
    temporary_data
  )
}
population_data$distribution <- factor(
  population_data$distribution,
  levels = distributions
)
ggplot(
  population_data,
  aes(x = value)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 60,
    fill = "#85002A",
    color = "white"
  ) +
  facet_wrap(
    ~ distribution,
    ncol = 2,
    scales = "free_y"
  ) +
  coord_cartesian(xlim = c(40, 200)) +
  labs(
    title = "Four Population Distributions",
    subtitle = "Each population has mean 100 and standard deviation 20",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------
# 3. Generate the sampling distributions
# ------------------------------------------------------------

sample_sizes <- c(3, 5, 50)
number_of_samples <- 10000

sampling_data <- data.frame()

for (distribution_name in distributions) {
  
  for (current_sample_size in sample_sizes) {
    
    sample_means <- replicate(
      number_of_samples,
      mean(
        draw_population(
          distribution = distribution_name,
          n = current_sample_size
        )
      )
    )
    
    temporary_data <- data.frame(
      sample_mean = sample_means,
      distribution = distribution_name,
      sample_size = paste0("n = ", current_sample_size)
    )
    
    sampling_data <- rbind(
      sampling_data,
      temporary_data
    )
  }
}

sampling_data$distribution <- factor(
  sampling_data$distribution,
  levels = distributions
)

sampling_data$sample_size <- factor(
  sampling_data$sample_size,
  levels = paste0("n = ", sample_sizes)
)

# ------------------------------------------------------------
# 4. Plot the sampling distributions
# ------------------------------------------------------------

ggplot(
  sampling_data,
  aes(x = sample_mean)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "#85002A",
    color = "white"
  ) +
  facet_grid(
    distribution ~ sample_size,
    scales = "free_y"
  ) +
  geom_vline(
    xintercept = population_mean,
    color = "#1769AA",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  coord_cartesian(xlim = c(60, 140)) +
  labs(
    title = "Sampling Distributions of the Sample Mean",
    subtitle = paste(
      "Each panel is based on",
      format(number_of_samples, big.mark = ","),
      "random samples"
    ),
    x = "Sample Mean",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)