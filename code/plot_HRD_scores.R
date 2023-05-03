library(ggplot2)
library(data.table)
library(gridExtra)
library(png)

HRD_scores = data.frame(read.csv('../data/HRD_scores_pan_cancer_annotated_typecorrect.csv'))

scores = c("HRD_sum","TAI","LOH","LST")

## Cancer types are labeled with their Project ID ##

##### HRD scores plots for pan cancer ####

# Boxplot of each score containing all cancertypes

n_obs <- table(HRD_scores$Project.ID)
for (score in scores){
  if (score == 'HRD_sum'){
    y_label = 'HRD sum'
  } else{
    y_label = score
  }
  score_plot = ggplot(HRD_scores,aes(x = reorder(Project.ID, -.data[[score]], FUN = median), y = .data[[score]], fill = Project.ID))  +
    geom_boxplot(show.legend = FALSE) + 
    stat_boxplot(geom='errorbar', linetype=1, width=0.5) + 
    theme(axis.text.x = element_text(angle = 75,hjust = 1.0), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab('Project ID') + 
    ylab(y_label) +
    scale_x_discrete(labels = function(x) paste0(x, " (n = ", n_obs[x],')'))
  print(score_plot)
  ggsave(paste('../data/figures_pan_cancer/pan_cancer/',score,'_bp.png',sep = ''),score_plot,width = 8.63, height = 5.71)
  
}





#### HRD scores plots with subgrouping by Type (Primary, Metastatic, Recurrent) ####

# Boxplot of each score containing all cancertypes, each split up into the three sample types (Primary, Recurrent, Metastatic)

n_obs <- table(HRD_scores$Project.ID)
n_obs_types <- table(HRD_scores$Project.ID, HRD_scores$Type)

for (score in scores){
  if (score == 'HRD_sum'){
    y_label = 'HRD sum'
  } else{
    y_label = score
  }
  score_plot = ggplot(HRD_scores,aes(x = reorder(Project.ID, -.data[[score]], FUN = median), y = .data[[score]], fill = Type))  +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 75,hjust = 1.0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab('Project ID') + 
    ylab(y_label) +
    scale_x_discrete(labels = function(x) {
      p <- n_obs_types[x, "Primary"]
      m <- n_obs_types[x, "Metastatic"]
      r <- n_obs_types[x, "Recurrent"]
      paste0(x, " (n=", p+m+r, ", P=", p, ", M=", m, ", R=", r, ")")
    })
  print(score_plot)
  ggsave(paste('../data/figures_pan_cancer/pan_cancer/',score,'_bp_grouped_type.png',sep = ''),score_plot,width = 8.63, height = 5.71)
}



#### Boxplot per cancer type with every score ####

# Creates a bocplot for each cancer type containing all 4 scores and saves them
for (i in unique(HRD_scores$Project.ID)) {
  df_subset <- HRD_scores[HRD_scores$Project.ID == i,]
  plot_data <- data.frame(Score = c(rep("LST", nrow(df_subset)), 
                                    rep("HRD_sum", nrow(df_subset)), 
                                    rep("LOH", nrow(df_subset)), 
                                    rep("TAI", nrow(df_subset))),
                          Value = c(df_subset$LST, df_subset$HRD_sum, df_subset$LOH, df_subset$TAI))
  
  plot = ggplot(plot_data, aes(x = Score, y = Value, fill=Score)) +
    geom_boxplot(show.legend = FALSE) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Scores") +
    ylab("Values") +
    ggtitle(i)
  print(plot)
  ggsave(paste('../data/figures_pan_cancer/bp_scores_per_cancer/',i,'.png',sep = ''))
}

# Next part combines the previous created boxplot per cancertype into one summary plot
# Get the plots
files = list.files(path = "../data/figures_pan_cancer/bp_scores_per_cancer/", pattern = NULL, all.files = FALSE,
                   full.names = TRUE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# Create the grid for the new plot
rl = lapply(files, png::readPNG)
gl = lapply(rl, grid::rasterGrob)

# Define parameters for the summary plot
n_plots <- length(gl)
n_rows <- ceiling(sqrt(n_plots))
n_cols <- ceiling(n_plots / n_rows)

widths <- rep(40, n_cols)
heights <- rep(40, n_rows)

total_width <- sum(widths)
total_height <- sum(heights)
output_width <- total_width * 0.1
output_height <- total_height * 0.1

# Plot the summary plot and save it
summuary_plot = arrangeGrob(grobs=gl, ncol=n_cols, nrow=n_rows, widths=widths, heights=heights)
ggsave("../data/figures_pan_cancer/summary_scores_plots.png",summuary_plot,width = output_width, height = output_height,limitsize = FALSE)


#### Distribution plots ####

# Only show Primary sample types
HRD_scores_primary = HRD_scores[HRD_scores$Type == 'Primary',]


# Create a density plot for each cancer type of the HRDsum and save it
for (i in unique(HRD_scores_primary$Project.ID)) {
  df_subset <- HRD_scores_primary[HRD_scores_primary$Project.ID == i,]
  
  plot = ggplot(df_subset, aes(x = as.numeric(HRD_sum))) +
    geom_density(alpha = 0.4) +
    labs(x = "HRD score", y = "Density", title = paste("HRD score Distribution of ",i,sep = ''))
  
  print(plot)
  ggsave(paste('../data/figures_pan_cancer/density_plots_per_cancer/',i,'.png',sep = ''))
}

# Next part combines the previous created boxplot per cancertype into one summary plot
# Get the plots
files = list.files(path = "../data/figures_pan_cancer/density_plots_per_cancer/", pattern = NULL, all.files = FALSE,
                   full.names = TRUE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# Create the grid for the new plot
rl = lapply(files, png::readPNG)
gl = lapply(rl, grid::rasterGrob)

# Define parameters for the summary plot
n_plots <- length(gl)
n_rows <- ceiling(sqrt(n_plots))
n_cols <- ceiling(n_plots / n_rows)

widths <- rep(40, n_cols)
heights <- rep(40, n_rows)

total_width <- sum(widths)
total_height <- sum(heights)
output_width <- total_width * 0.1
output_height <- total_height * 0.1

# Plot the summary plot and save it
summuary_plot = arrangeGrob(grobs=gl, ncol=n_cols, nrow=n_rows, widths=widths, heights=heights)
ggsave("../data/figures_pan_cancer/summary_density_plots.png",summuary_plot,width = output_width, height = output_height,limitsize = FALSE)


# Creates a plot containing all cancer types and their distribution of the HRDsum
ggplot(HRD_scores_primary, aes(x = as.numeric(HRD_sum), fill = Project.ID)) +
  geom_density(alpha = 0.4) +
  labs(x = "HRD score", y = "Density", title = "HRD score Distribution by Cancer Type") +
  scale_fill_discrete(name = "Cancer type")



#### end ####

## Cancer types are labeled with their Study names ## 

#### HRD scores plots for pan cancer ####

# Creates a boxplot for each score containing all cancertypes
n_obs <- table(HRD_scores$Study.Name)
for (score in scores){
  if (score == 'HRD_sum'){
    y_label = 'HRD sum'
  } else{
    y_label = score
  }
  score_plot = ggplot(HRD_scores,aes(x = reorder(Study.Name, -.data[[score]], FUN = median), y = .data[[score]], fill = Study.Name))  +
    geom_boxplot(show.legend = FALSE) + 
    stat_boxplot(geom='errorbar', linetype=1, width=0.5) + 
    theme(axis.text.x = element_text(angle = 75,hjust = 1.0), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab('Cancer type') + 
    ylab(y_label) +
    scale_x_discrete(labels = function(x) paste0(x, " (n = ", n_obs[x],')'))
  print(score_plot)
  ggsave(paste('../data/figures_pan_cancer/pan_cancer_studyname/',score,'_bp.png',sep = ''),score_plot,width = 8.63, height = 5.71)
  
}

#### HRD scores plots with subgrouping by Type (Primary, Metastatic, Recurrent) ####

# Boxplot of each score containing all cancertypes, each split up into the three sample types (Primary, Recurrent, Metastatic)

n_obs <- table(HRD_scores$Study.Name)
n_obs_types <- table(HRD_scores$Study.Name, HRD_scores$Type)

for (score in scores){
  if (score == 'HRD_sum'){
    y_label = 'HRD sum'
  } else{
    y_label = score
  }
  score_plot = ggplot(HRD_scores,aes(x = reorder(Study.Name, -.data[[score]], FUN = median), y = .data[[score]], fill = Type))  +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 75,hjust = 1.0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab('Cancer type') + 
    ylab(y_label) +
    scale_x_discrete(labels = function(x) {
      p <- n_obs_types[x, "Primary"]
      m <- n_obs_types[x, "Metastatic"]
      r <- n_obs_types[x, "Recurrent"]
      paste0(x, " (n=", p+m+r, ", P=", p, ", M=", m, ", R=", r, ")")
    })
  print(score_plot)
  ggsave(paste('../data/figures_pan_cancer/pan_cancer_studyname/',score,'_bp_grouped_type.png',sep = ''),score_plot,width = 8.63, height = 5.71)
}

#### end ####