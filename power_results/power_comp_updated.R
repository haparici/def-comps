library(tidyverse)
library(brms)
library(tidybayes)
library(broom)
library(plyr)
library(rstan)

options(mc.cores = parallel::detectCores())

#setwd("C:/Users/kertk/Desktop/MIT/Computational Psycholinguistics Lab/Haddock Experiment/Power")
#setwd("~/Desktop/power")


args = commandArgs(trailingOnly=TRUE);

subjN_list <- c(100, 150, 200, 250, 300, 350, 400, 450, 500, 600)
itemsN_list <- c(14, 16, 24, 28, 32, 36, 40)

subjN <- subjN_list[as.numeric(args[1])];
itemN <- itemsN_list[as.numeric(args[2])];

#subjN <- 250
#itemN <- 14

print(paste("1. Currently simulating", subjN, "subjects!"));
print(paste("2. Currently simulating", itemN, "items!"));

# 100
iterations <- 3000 #3000
simulations <- 3 # 5


# PILOT PREPARATIONS.

pilot <- read_csv("pilotdata.csv") # extract only the target items useful to analysis

preprocess <- ddply(pilot, .(ID), plyr::summarize, meanScore=mean(response), sd=sd(response))


pilot <- merge(pilot, preprocess, by=c("ID"));
pilot$zScore <- ifelse(pilot$sd == 0, 0, (pilot$response - pilot$meanScore) / pilot$sd);

pilot <- subset(pilot, type=="target" & condition=="PROGRESSIVE" & (cardinality == "3" | cardinality == "4"))
pilot <- subset(pilot, !(pilot$shape == "arrow" | pilot$shape == "line"));
pilot$row <- 1:nrow(pilot);




# ITEM FIXES

segments <- strsplit(pilot$sentence, " ");
df <- data.frame(matrix(unlist(segments), nrow=length(segments), byrow=T))['X4']
colnames(df)[which(names(df) == "X4")] <- "adjType"
df$row <- 1:nrow(df);

adjectives <- c("big", "small", "short", "tall")


for (a in adjectives) {
  df$adjType = ifelse(grepl(a, df$adjType) == 1, a, df$adjType);
}

pilot <- merge(pilot, df, by=c("row"), all=TRUE);

pilot$item <- paste(pilot$shape, "-", pilot$adjType, sep="")

# s <- ggplot(data=subset(pilot, pilot$condition == "CRISP"), aes(x=cardinality, y=response, fill=adjective)) +
#   facet_wrap(~display) +
#   stat_summary(aes(fill = factor(adjective)), fun=mean, geom="bar", position = position_dodge(0.9))+
#   stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=5,
#                vjust = 1.6, position = s(0.9)) +
#   ggtitle("Preference Scores by Item (for Progressive)") +
#   #geom_point(position=position_dodge(.8)) +
#   ylim(0, 10) +
#   labs(y="Preference Score", x = "Sentence type",  fill="Determiner combination") +
#   theme_minimal()

# removal of star

pilot <- subset(pilot, (pilot$shape != "square" & pilot$shape != "hexagon" & pilot$shape != "star"));


pilot <- pilot %>%
  mutate(adjective = factor(adjective, levels = c("COMPARATIVE", "SUPERLATIVE")), # turn these columns into R factors
        cardinality = factor(cardinality, levels = c("3", "4")))

# contrasts(pilot$adjective) <- contr.sum(2) # set the contrasts between the levels of each factor
# contrasts(pilot$cardinality) <- contr.sum(2)




# pilot model

model <- brm(
  zScore ~ cardinality * adjective + (1 + cardinality * adjective || ID)
  + (1 + cardinality * adjective || shape), # need to pull out the adjective to get shape-adj combo
  data = pilot,
  family = "gaussian", # continuous space
  iter = iterations, # defaults to 2000
  cores = 4,
  control = list(adapt_delta = 0.95), # increasing this decreases divergent transitions in the model when sampling
  silent = T,
  refresh = 0
)

print("Finished constructing model for pilot data!")


# predicted data from the pilot model / data

predicted_data <- add_predicted_draws(model = model, newdata = pilot, n = 1, prediction = "predictedZScore") %>%
  mutate(ID = paste(ID, "new", sep = "_"))


print("Finished generating data from the pilot model!");


predicted_model <- brm(
  predictedZScore ~ cardinality * adjective + (1 + cardinality * adjective || ID)
  + (1 + cardinality * adjective || item),
  data = predicted_data,
  family = "gaussian",
  iter = iterations,
  cores = 4,
  control = list(adapt_delta = 0.95),
  silent = T,
  refresh = 0
)



print("Finished generating preliminary predicted model!");

summary(predicted_model)


generate_item <- function(N, N_items){
  items <- c(unique(pilot$item), paste("item", 1:20, sep="_"))
  targets <- sample(items[1:N_items], 4, replace = F)
  return(targets)
}

nextID = length(unique(pilot$ID)) + 1;

generate_data <- function(N, pilot_model, N_items) {
  
  adjective <- rep(c("COMPARATIVE", "SUPERLATIVE"), each = 2)
  cardinality <- rep(c("3", "4"), times = 2)
  
  frame <- tibble(
    ID = nextID:(nextID + N - 1), # ask about this--repeated worker ID's?
    item = map(ID, generate_item, N_items),
  ) %>%
    unnest(cols = item) %>%
    mutate(
      adjective = as.factor(rep(adjective, times = N)),
      cardinality = as.factor(rep(cardinality, times = N))
    )
  
  nextID <- nextID + N;
  
  data <- add_predicted_draws(model=pilot_model,
                              newdata = frame,
                              allow_new_levels = T, # how does this allow for sampling new IDs?
                              sample_new_levels = "gaussian",
                              n = 1, 
                              prediction = "predictedZScore")
  return(data)
}

stream_out <- paste("results/power_analysis_res_", subjN, "subj_", itemN, "items_", iterations,  "iter_", simulations, "sim.csv", sep="")

  
simulation <- function(seed, N, items) {
    
    set.seed(seed)
    
    print(paste("iteration:", seed, " , subjects:", N, "items:", items, sep=" "))
    
    data <- generate_data(N, pilot_model = model, N_items = items) %>%
      mutate(adjective = factor(adjective, levels = c("COMPARATIVE", "SUPERLATIVE")),
             cardinality = factor(cardinality, levels = c("3", "4")))
    
    # contrasts(data$adjective) <- contr.sum(2)
    # contrasts(data$cardinality) <- contr.sum(2)
    
    print("data generated!")
    
    
    
    updated_model <- update(predicted_model,
                            newdata = data,
                            seed = seed,
                            cores = 4,
                            silent = T,
                            refresh = 0);
    
    res2 <- updated_model %>% spread_draws(b_Intercept, b_adjectiveSUPERLATIVE, b_cardinality4, `b_cardinality4:adjectiveSUPERLATIVE`) %>%
      select(b_Intercept, b_adjectiveSUPERLATIVE, b_cardinality4, `b_cardinality4:adjectiveSUPERLATIVE`) %>%
      gather(key, val) %>% ddply(
        .(key), plyr::summarize, mean = mean(val), lower = quantile(val, probs = 0.025), upper = quantile(val, probs = 0.975)
      ) %>%
      tibble() %>%
      mutate(
        n.subj = N,
        n.item = items, 
        seed = seed
      ) %>%
      
      
      
      # write out results for each subject N and each seed
      write_csv(., stream_out, append = T, col_names = (seed == 1)) %>% select(-n.subj, -seed, -n.item)
    
    hyp <- c(hyp = "cardinality4 + cardinality4:adjectiveSUPERLATIVE = 0")
    res <- hypothesis(updated_model, hyp)
    append <- data.frame(key = "b_cardinality4 + b_cardinality4:adjectiveSUPERLATIVE", mean=res$hypothesis$Estimate, lower=res$hypothesis$CI.Lower, upper=res$hypothesis$CI.Upper) %>%
      tibble() %>%
      mutate(
        n.subj = subjN,
        n.item = itemN, 
        seed = seed
      ) %>%
      write_csv(., stream_out, append = T, col_names = FALSE)
    
    return(res2);
  }
  

sim.power <- function(n.subj, n.sim, n.item) {
  sim <- tibble(seed = 1:n.sim) %>%
    mutate(
      tidy = map(seed, simulation, N = n.subj, items = n.item)
    ) %>%
    unnest(tidy)
}

analyse_power <- tibble(n.subj = subjN, n.item = itemN) %>%
  mutate(
    tidy = map(n.subj, sim.power, n.sim = simulations, n.item)
  ) %>%
  unnest(tidy)

write_csv(analyse_power, paste("results/power_analysis_res_full_", subjN, "subj_", itemN, "items_", iterations,  "iterations_", simulations, "sim.csv", sep=""), append = T, col_names = T)

analyse_power %>%
  filter(key == "b_cardinality4:adjectiveSUPERLATIVE") %>%
  mutate(check = ifelse((lower > 0) | (upper < 0 ), 1, 0)) %>%
  group_by(n.subj, n.item) %>%
  summarise(power = mean(check)) -> analyse_power_summary

analyse_power_summary %>% write_csv(paste("results/power_analysis_res_final_", subjN, "subj_", itemN, "items_", iterations, "iterations_", simulations, "sim_summary.csv", sep = ""))


