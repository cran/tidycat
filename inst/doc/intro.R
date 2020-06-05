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
   glm(cbind(ncases, ncontrols) ~ agegp + tobgp + alcgp, data = ., family = binomial())
# tidy
tidy(m0)

## -----------------------------------------------------------------------------
library(tidycat)
m0 %>%
  tidy() %>%
  tidy_categorical(m = m0, include_reference =  FALSE)

## -----------------------------------------------------------------------------
m0 %>%
  tidy(exponentiate = TRUE) %>%
  tidy_categorical(m = m0, exponentiate = TRUE, reference_label = "Baseline") %>%
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
  tidy_categorical(m = m0, include_reference = FALSE) %>%
  slice(-1)

d0 %>%
  select(-(3:5))

ggplot(data = d0,
        mapping = aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high,
                      colour = variable)) +
   coord_flip() +
   geom_hline(yintercept = 0, linetype = "dashed") +
   geom_pointrange()

## ---- fig.width=6, fig.height=4-----------------------------------------------
d0 <- m0 %>%
  tidy(conf.int = TRUE) %>%
  tidy_categorical(m = m0) %>%
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

## -----------------------------------------------------------------------------
m1 <- mtcars %>%
  mutate(engine = recode_factor(vs, `0` = "straight", `1` = "V-shaped"),
         transmission = recode_factor(am, `0` = "automatic", `1` = "manual")) %>%
  lm(mpg ~ as.factor(cyl) + wt * hp + wt * transmission + engine * transmission , data = .)

tidy(m1)

## -----------------------------------------------------------------------------
d1 <- m1 %>%
  tidy(conf.int = TRUE) %>%
  tidy_categorical(m = m1, n_level = TRUE) %>%
  slice(-1)

d1 %>%
  select(-(2:7))

## ---- fig.width=6, fig.height=6-----------------------------------------------
ggplot(data = d1,
        mapping = aes(x = level, y = estimate, colour = reference,
                      ymin = conf.low, ymax = conf.high)) +
   facet_col(facets = "variable", scales = "free_y", space = "free") +
   coord_flip() +
   geom_hline(yintercept = 0, linetype = "dashed") +
   geom_pointrange()

## ---- fig.width=6, fig.height=5-----------------------------------------------
d1 %>%
  dplyr::filter(n_level > 0 | !is.na(term)) %>%
  ggplot(mapping = aes(x = level, y = estimate, colour = reference,
                       ymin = conf.low, ymax = conf.high)) +
  facet_col(facets = "variable", scales = "free_y", space = "free") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange()

