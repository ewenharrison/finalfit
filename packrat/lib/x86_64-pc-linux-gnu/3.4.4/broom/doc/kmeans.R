## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(purrr)
library(tibble)
library(tidyr)

set.seed(27)

centers <- tibble(
  cluster = factor(1:3), 
  num_points = c(100, 150, 50),  # number points in each cluster
  x1 = c(5, 0, -3),              # x1 coordinate of cluster center
  x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
)

labelled_points <- centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  select(-num_points) %>% 
  unnest(x1, x2)

ggplot(labelled_points, aes(x1, x2, color = cluster)) +
  geom_point()

## ------------------------------------------------------------------------
points <- labelled_points %>% 
  select(-cluster)

kclust <- kmeans(points, centers = 3)
kclust
summary(kclust)

## ------------------------------------------------------------------------
library(broom)

augment(kclust, points)

## ------------------------------------------------------------------------
tidy(kclust)

## ------------------------------------------------------------------------
glance(kclust)

## ------------------------------------------------------------------------
kclusts <- tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )

kclusts

## ------------------------------------------------------------------------
clusters <- kclusts %>%
  unnest(tidied)

assignments <- kclusts %>% 
  unnest(augmented)

clusterings <- kclusts %>%
  unnest(glanced, .drop = TRUE)

## ------------------------------------------------------------------------
p1 <- ggplot(assignments, aes(x1, x2)) +
  geom_point(aes(color = .cluster)) + 
  facet_wrap(~ k)
p1

## ------------------------------------------------------------------------
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

## ------------------------------------------------------------------------
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line()

