# HOUSEKEEPING ----
data_filename <- "busn320-ch07-flights.csv"

# Locate the script when run with source() or RStudio's Source button
source_files <- lapply(sys.frames(), function(frame) frame$ofile)
source_files <- Filter(
  function(path) is.character(path) && length(path) == 1 && nzchar(path),
  source_files
)

if (length(source_files) > 0) {
  script_path <- source_files[[length(source_files)]]
} else {
  # Locate the script when run with Rscript
  file_arg <- grep(
    "^--file=",
    commandArgs(trailingOnly=FALSE),
    value=TRUE
  )
  
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[1])
  } else if (
    requireNamespace("rstudioapi", quietly=TRUE) &&
    rstudioapi::isAvailable()
  ) {
    # Useful when running selected code from a saved RStudio script
    script_path <- rstudioapi::getSourceEditorContext()$path
  } else {
    script_path <- ""
  }
}

if (length(script_path) == 0 || !nzchar(script_path)) {
  # For code pasted into a Console, run R from the script/CSV folder
  project_dir <- getwd()
} else {
  project_dir <- dirname(normalizePath(script_path, mustWork=TRUE))
}

flight_file <- file.path(project_dir, data_filename)

if (!file.exists(flight_file)) {
  stop(
    "Could not find ", data_filename, " in:\n",
    project_dir,
    "\nSave the CSV in the same folder as the R script."
  )
}

# PACKAGES ----
library(ggplot2)

# IMPORT THE DATA ----
flight_data <- read.csv(flight_file)

if (!"AirTime" %in% names(flight_data)) {
  stop("The CSV must contain a column named AirTime.")
}

# EXTRACT AIRTIME ----
air_time <- as.numeric(flight_data$AirTime)

# Remove missing or otherwise invalid observations
air_time <- air_time[
  !is.na(air_time) &
    is.finite(air_time) &
    air_time > 0
]

# Examine the resulting population
length(air_time)
summary(air_time)

# POPULATION CHARACTERISTICS ----
population_size <- length(air_time)

if (population_size < 500) {
  stop("At least 500 valid flight times are needed for the largest sample.")
}

population_mean <- mean(air_time)

# Calculate the population standard deviation using N as the divisor
population_sd <- sqrt(
  mean((air_time-population_mean)^2)
)

population_size
population_mean
population_sd

# PLOTTING THE POPULATION ----
population_data <- data.frame(AirTime=air_time)

population_plot <- ggplot(population_data,aes(x=AirTime)) +
  geom_histogram(bins=40,fill="#85002A",color="white") +
  geom_vline(
    xintercept=population_mean,
    color="#1769AA",
    linewidth=1,
    linetype="dashed"
  ) +
  labs(
    title="Population Distribution of Flight Times",
    subtitle=paste0(
      "Population mean = ",
      round(population_mean,1),
      " minutes"
    ),
    x="Flight Time (Minutes)",
    y="Number of Flights"
  ) +
  theme_minimal(base_size=14) +
  theme(
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5)
  )

population_plot

# SELECTING ONE RANDOM SAMPLE ----
set.seed(320)

sample_size <- 100
one_sample <- sample(air_time,size=sample_size,replace=FALSE)
one_sample_mean <- mean(one_sample)

one_sample_mean

one_sample_data <- data.frame(AirTime=one_sample)

sample_plot <- ggplot(one_sample_data,aes(x=AirTime)) +
  geom_histogram(bins=20,fill="#85002A",color="white") +
  geom_vline(
    xintercept=one_sample_mean,
    color="#1769AA",
    linewidth=1,
    linetype="dashed"
  ) +
  labs(
    title="Distribution of One Random Sample",
    subtitle=paste0(
      "Sample size = ",sample_size,
      "   Sample mean = ",round(one_sample_mean,1)," minutes"
    ),
    x="Flight Time (Minutes)",
    y="Number of Flights"
  ) +
  theme_minimal(base_size=14) +
  theme(
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5)
  )

sample_plot

# SAMPLING DISTRIBUTION WITH SAMPLE SIZE 100 ----
set.seed(320)

sample_size <- 100
number_of_samples <- 10000

sample_means <- replicate(
  number_of_samples,
  mean(sample(air_time,size=sample_size,replace=FALSE))
)

sampling_data <- data.frame(SampleMean=sample_means)

sampling_mean <- mean(sample_means)
sampling_sd <- sd(sample_means)

sampling_mean
sampling_sd

sampling_plot <- ggplot(sampling_data,aes(x=SampleMean)) +
  geom_histogram(
    aes(y=after_stat(density)),
    bins=35,
    fill="#85002A",
    color="white"
  ) +
  geom_vline(
    xintercept=population_mean,
    color="#1769AA",
    linewidth=1,
    linetype="dashed"
  ) +
  labs(
    title="Sampling Distribution of the Sample Mean",
    subtitle=paste0(
      format(number_of_samples,big.mark=","),
      " random samples of ",sample_size," flights"
    ),
    x="Sample Mean Flight Time",
    y="Density"
  ) +
  theme_minimal(base_size=14) +
  theme(
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5)
  )

sampling_plot

# COMPARISON WITH THE THEORETICAL SAMPLING DISTRIBUTION ----
theoretical_se <- (population_sd/sqrt(sample_size)) *
  sqrt((population_size-sample_size)/(population_size-1))

theoretical_se

comparison_plot <- ggplot(sampling_data,aes(x=SampleMean)) +
  geom_histogram(
    aes(y=after_stat(density)),
    bins=35,
    fill="#85002A",
    color="white"
  ) +
  stat_function(
    fun=dnorm,
    args=list(mean=population_mean,sd=theoretical_se),
    color="#1769AA",
    linewidth=1.2
  ) +
  geom_vline(
    xintercept=population_mean,
    color="#1769AA",
    linewidth=0.8,
    linetype="dashed"
  ) +
  labs(
    title="Sampling Distribution of the Sample Mean",
    subtitle=paste0(
      format(number_of_samples,big.mark=","),
      " random samples of ",sample_size," flights"
    ),
    x="Sample Mean Flight Time",
    y="Density"
  ) +
  theme_minimal(base_size=14) +
  theme(
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5)
  )

comparison_plot

# COMPARING THE RESULTS ----
comparison <- data.frame(
  Quantity=c(
    "Population mean",
    "Mean of simulated sample means",
    "Theoretical standard error",
    "SD of simulated sample means"
  ),
  Value=c(
    population_mean,
    sampling_mean,
    theoretical_se,
    sampling_sd
  )
)

comparison

# SAMPLING DISTRIBUTIONS WITH DIFFERENT SAMPLE SIZES ----
set.seed(320)

sample_sizes <- c(2,25,100,500)
number_of_samples <- 10000

sampling_size_data <- do.call(
  rbind,
  lapply(sample_sizes,function(current_sample_size) {
    
    current_sample_means <- replicate(
      number_of_samples,
      mean(sample(
        air_time,
        size=current_sample_size,
        replace=FALSE
      ))
    )
    
    data.frame(
      SampleMean=current_sample_means,
      SampleSize=current_sample_size
    )
  })
)

# Create ordered labels for the different sample sizes
sampling_size_data$SampleSize <- factor(
  sampling_size_data$SampleSize,
  levels=sample_sizes,
  labels=paste0("n = ",sample_sizes)
)

# Examine the resulting sampling distributions
sampling_size_summary <- aggregate(
  SampleMean~SampleSize,
  data=sampling_size_data,
  FUN=function(x) c(Mean=mean(x),StandardError=sd(x))
)

sampling_size_summary

# GRAPH ----
sample_size_plot <- ggplot(sampling_size_data,aes(x=SampleMean)) +
  geom_histogram(
    aes(y=after_stat(density)),
    bins=35,
    fill="#85002A",
    color="white"
  ) +
  geom_vline(
    xintercept=population_mean,
    color="#1769AA",
    linewidth=0.8,
    linetype="dashed"
  ) +
  facet_wrap(~SampleSize,ncol=2,scales="free") +
  labs(
    title="Sampling Distributions for Different Sample Sizes",
    subtitle=paste0(
      format(number_of_samples,big.mark=","),
      " random samples for each sample size"
    ),
    x="Sample Mean Flight Time",
    y="Density"
  ) +
  theme_minimal(base_size=14) +
  theme(
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5),
    strip.text=element_text(size=13,face="bold")
  )

sample_size_plot

# COMPARING SAMPLE SIZES 100 AND 500 ----
large_sample_data <- sampling_size_data[
  sampling_size_data$SampleSize %in% c("n = 100","n = 500"),
]

large_sample_plot <- ggplot(large_sample_data,aes(x=SampleMean)) +
  geom_histogram(
    aes(y=after_stat(density)),
    bins=35,
    fill="#85002A",
    color="white"
  ) +
  geom_vline(
    xintercept=population_mean,
    color="#1769AA",
    linewidth=0.8,
    linetype="dashed"
  ) +
  facet_wrap(~SampleSize,ncol=1,scales="free_y") +
  labs(
    title="Effect of Sample Size on the Sampling Distribution",
    subtitle=paste0(
      format(number_of_samples,big.mark=","),
      " random samples for each sample size"
    ),
    x="Sample Mean Flight Time",
    y="Density"
  ) +
  theme_minimal(base_size=14) +
  theme(
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5),
    strip.text=element_text(size=13,face="bold")
  )

large_sample_plot

# SAVING THE FIGURES ----
ggsave(
  filename=file.path(project_dir,"busn-320-ch07-panel02-fig01.png"),
  plot=population_plot,width=8,height=8,dpi=300
)

ggsave(
  filename=file.path(project_dir,"busn-320-ch07-panel02-fig02.png"),
  plot=sample_plot,width=8,height=8,dpi=300
)

ggsave(
  filename=file.path(project_dir,"busn-320-ch07-panel02-fig03.png"),
  plot=sampling_plot,width=8,height=8,dpi=300
)

ggsave(
  filename=file.path(project_dir,"busn-320-ch07-panel02-fig04.png"),
  plot=comparison_plot,width=8,height=8,dpi=300
)

ggsave(
  filename=file.path(project_dir,"busn-320-ch07-panel02-fig05.png"),
  plot=sample_size_plot,width=8,height=8,dpi=300
)

ggsave(
  filename=file.path(project_dir,"busn-320-ch07-panel02-fig06.png"),
  plot=large_sample_plot,width=8,height=8,dpi=300
)