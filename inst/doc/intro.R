## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(dplyr)
library(broom)
m0 <- esoph %>%
   mutate_if(is.factor, ~factor(., ordered = FALSE)) %>%
   glm(cbind(ncases, ncontrols) ~ agegp + tobgp + alcgp, data = .,
         family = binomial())
# tidy
tidy(m0)

## -----------------------------------------------------------------------------
library(tidycat)
m0 %>%
  tidy() %>%
  tidy_categorical(m = m0)

## -----------------------------------------------------------------------------
m0 %>%
  tidy(exponentiate = TRUE) %>%
  tidy_categorical(m = m0, exponentiate = TRUE, 
                   exclude_reference = FALSE, reference_label = "Baseline") %>%
  select(-statistic, -p.value)

## ---- fig.width=6, fig.height=4-----------------------------------------------
# store parameter estimates and confidence intervals (except for the intercept)
d0 <- m0 %>%
  tidy(conf.int = TRUE) %>%
  slice(-1)
d0

library(ggplot2)
library(tidyr)
ggplot(data = d0,
        mapping = aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
   coord_flip() +
   geom_hline(yintercept = 0, linetype = "dashed") +
   geom_pointrange()

## ---- fig.width=6, fig.height=4-----------------------------------------------
d0 <- m0 %>%
  tidy(conf.int = TRUE) %>%
  tidy_categorical(m = m0, exclude_reference = FALSE, reference_label = "Baseline") %>%
  slice(-1)

d0 %>%
  select(-(3:5))

library(ggforce)
ggplot(data = d0,
        mapping = aes(x = level, y = estimate, colour = reference,
                      ymin = conf.low, ymax = conf.high)) +
   facet_col(facets = vars(variable), scales = "free_y", space = "free") +
   coord_flip() +
   geom_hline(yintercept = 0, linetype = "dashed") +
   geom_pointrange()

## ---- fig.width=6, fig.height=4-----------------------------------------------
ggplot(data = d0,
      mapping = aes(x = level, y = estimate,
                    ymin = conf.low, ymax = conf.high,
                    colour = reference)) +
 facet_row(facets = vars(variable), scales = "free_x", space = "free") +
 geom_hline(yintercept = 0, linetype = "dashed") +
 geom_pointrange() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

