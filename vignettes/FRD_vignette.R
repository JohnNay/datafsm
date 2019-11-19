## ----setup, include=FALSE------------------------------------------------
is_check <- ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_",
             "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))
do_cache <- FALSE
knitr::opts_chunk$set(eval = !is_check, echo = TRUE, cache = do_cache, 
                      warning=FALSE, message=FALSE, error=FALSE,
                      fig.width = 6, fig.height = 4.25,
                      fig.align = "center")


## ----initialize----------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(pander)
library(datafsm)
set.seed(1533406114)

## ----prepare_data--------------------------------------------------------
data(NV_games, package = "datafsm")

group_df <- list(g1 = 15, g2 = 16, g3 = 17, g4 = 14, g5 = 13, g6 = 12) %>%
  as_tibble() %>% pivot_longer(cols = everything(), names_to = "label", 
                               values_to = "group") %>%
  mutate(group = as.integer(group))

prepped <- NV_games %>% transmute(
  group = as.integer(group),
  outcome = as.integer(my.decision == "coop") + 1,
  period = as.integer(period),
  my.decision1 = as.integer(my.decision1 == 1),
  other.decision1 = as.integer(other.decision1 == 1)
) %>%
  filter(group %in% group_df$group) %>%
  full_join(group_df, by = "group") %>%
  arrange(label) %>%
  select(-group) %>% rename(group = label)

group_indices <- prepped$group %>% unique() %>% sort()
group_seeds <- sample.int(.Machine$integer.max, length(group_indices)) %>%
  set_names(as.character(group_indices))

patterns <- NV_games %>% select(group, error, r, t) %>%
  mutate(group = as.integer(group)) %>%
  full_join(group_df, by = "group") %>%
  select(-group) %>% rename(group = label) %>%
  filter(group %in% group_indices) %>%
  group_by(group) %>% distinct() %>% ungroup() %>%
  arrange(group)


## ----analyze_data, message=FALSE-----------------------------------------
res <- lapply(as.list(group_indices), function(x){ 
  message("\nAnalyzing group ", x, "\n")
  prepped %>% filter(group == x) %>% select(-group) %>%
    evolve_model(parallel = TRUE, 
                 seed = group_seeds[[as.character(x)]],
                 verbose = FALSE)
})

## ----plot_results--------------------------------------------------------
names(res) <- group_indices
tl = c("dd", "cd", "dc", "cc")
for(name in names(res)){
  pattern <- patterns %>% filter(group == name)
  this_res <- res[[name]]
  plot(this_res, 
       action_label = ifelse(action_vec(this_res)==1, "C", "D"), 
       transition_label = tl,
       maintitle = paste0("Error: ", pattern$error, 
                          ", Reward: ", pattern$r,
                          ", Temptation: ", pattern$t,
                          "\n In-sample accuracy (n= ", 
                          formatC(sum(prepped$group==name),
                                  big.mark=',',digits=0),") : ",
                          round(best_performance(this_res),2)))

   barplot(this_res, names.arg=tl)
}

## ----model_comparison----------------------------------------------------
c_minus_d <- res %>% map_dbl(~varImp(.x)[4] - varImp(.x)[1])
df <- c_minus_d %>% as.list() %>% as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "group", values_to = "c_minus_d") %>% 
  full_join(patterns, by = "group") %>% 
  select(error, r, t, c_minus_d) %>%
  rename(Error = error, Reward = r, Temptation = t,
         "CC Imp. Re. to DD" = c_minus_d)
pander(df)

