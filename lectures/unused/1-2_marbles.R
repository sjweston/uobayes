
# Counting possibilities

Suppose we have a bag that contains 4 marbles, and these marbles come in two colors: blue and white. We don't know how many of the marbles are blue and how many are white. There are therefore five possibilities:

```{r, echo = F, fig.retina = 3, fig.height = 5}
d <-
  tibble(p1 = 0,
         p2 = rep(1:0, times = c(1, 3)),
         p3 = rep(1:0, times = c(2, 2)),
         p4 = rep(1:0, times = c(3, 1)),
         p5 = 1)

d %>% 
  set_names(1:5) %>% 
  mutate(x = 1:4) %>% 
  pivot_longer(-x, names_to = "possibility") %>% 
  mutate(value = value %>% as.character(),
         possibility = as.numeric(possibility)) %>% 
  
  ggplot(aes(x = x, y = possibility, fill = value)) +
  geom_point(shape = 21, size = 10) +
  scale_fill_manual(values = c("white", "navy")) +
  scale_x_discrete(NULL, breaks = NULL) +
  scale_y_reverse() +
  labs(y = NULL,
       title = "Possible combinations") +
  theme_cowplot(font_size = 20) +
  theme(legend.position = "none", 
        axis.line = element_blank(),
        axis.ticks = element_blank())
```

---

Suppose we draw three marbles from the bag. What is every possible outcome?

```{r, echo = F}
d <-
  tibble(position = c((1:4^1) / 4^0, 
                      (1:4^2) / 4^1, 
                      (1:4^3) / 4^2),
         draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
         fill     = rep(c("b", "w"), times = c(1, 3)) %>% 
           rep(., times = c(4^0 + 4^1 + 4^2)))

# these will connect the dots from the first and second draws
lines_1 <-
  tibble(x    = rep(1:4, each = 4),
         xend = ((1:4^2) / 4),
         y    = 1,
         yend = 2)
# these will connect the dots from the second and third draws
lines_2 <-
  tibble(x    = rep((1:4^2) / 4, each = 4),
         xend = (1:4^3) / (4^2),
         y    = 2,
         yend = 3)
d <- d %>% 
  mutate(denominator = ifelse(draw == 1, .5,
                              ifelse(draw == 2, .5 / 4,
                                     .5 / 4^2))) %>% 
  mutate(position = position - denominator)

lines_1 <- lines_1 %>% 
  mutate(x    = x - 0.5,
         xend = xend - 0.5 / 4^1)
lines_2 <- lines_2 %>% 
  mutate(x    = x - 0.5 / 4^1,
         xend = xend - 0.5 / 4^2)

d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 4) +
  scale_fill_manual(values = c("navy", "white")) +
  scale_x_continuous(NULL, limits = c(0, 4), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3), breaks = NULL) +
  coord_polar() +
  theme_cowplot() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.line = element_blank())

```

---

We draw three marbles from the bag: 

```{r, fig.retina=3, fig.height=3, echo = F}
draw <-
  tibble(p1 = c(1,0,1))

draw %>% 
  set_names(1) %>% 
  mutate(x = 1:3) %>% 
  pivot_longer(-x, names_to = "possibility") %>% 
  mutate(value = value %>% as.character(),
         possibility = as.numeric(possibility)) %>% 
  
  ggplot(aes(x = x, y = possibility, fill = value)) +
  geom_point(shape = 21, size = 10) +
  scale_fill_manual(values = c("white", "navy")) +
  scale_x_discrete(NULL, breaks = NULL, labels = NULL) +
  scale_y_continuous() +
  labs(y = NULL) +
  theme_cowplot(font_size = 20) +
  theme(legend.position = "none", 
        axis.line = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank())
```

---

Now we count how frequently this exact sequence arises:

```{r, fig.retina=3, fig.height=8, echo = F}
lines_1 <-
  lines_1 %>% 
  mutate(remain = c(rep(0:1, times = c(1, 3)),
                    rep(0,   times = 4 * 3)))
lines_2 <-
  lines_2 %>% 
  mutate(remain = c(rep(0,   times = 4),
                    rep(1:0, times = c(1, 3)) %>% rep(., times = 3),
                    rep(0,   times = 12 * 4)))
d <-
  d %>% 
  mutate(remain = c(rep(1:0, times = c(1, 3)),
                    rep(0:1, times = c(1, 3)),
                    rep(0,   times = 4 * 4),
                    rep(1:0, times = c(1, 3)) %>% rep(., times = 3),
                    rep(0,   times = 12 * 4))) 
# finally, the plot:
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               linewidth = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               linewidth = 1/3) +
  geom_point(aes(fill = fill, alpha = remain %>% as.character()),
             shape = 21, size = 4) +
  # it's the alpha parameter that makes elements semitransparent
scale_fill_manual(values = c("navy", "white")) +
  scale_alpha_manual(values = c(1/5, 1)) +
  scale_x_continuous(NULL, limits = c(0, 4), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3), breaks = NULL) +
  coord_polar() +
  theme_cowplot(font_size = 20) +
  theme(legend.position = "none", 
        axis.line = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank())
```

---
  
  This is only one of the possible combinations of white and blue marbles in the bag, so we need to recreate this for all possibilities. Because there's at least one blue and at least one white marble, we can rule out the possibilities that are all white and all blue, leaving us with three possible combinations of white and blue marbles.

.pull-left[

```{r,echo = F, fig.retina=3, fig.height=8}
d <-
  tibble(position = c((1:4^1) / 4^0, 
                      (1:4^2) / 4^1, 
                      (1:4^3) / 4^2),
         draw     = rep(1:3, times = c(4^1, 4^2, 4^3)))


  d <-
  d %>% 
  bind_rows(
    d, d
  ) %>% 
  # here are the fill colors
  mutate(fill = c(rep(c("w", "b"), times = c(1, 3)) %>% rep(., times = c(4^0 + 4^1 + 4^2)),
                  rep(c("w", "b"), each  = 2)       %>% rep(., times = c(4^0 + 4^1 + 4^2)),
                  rep(c("w", "b"), times = c(3, 1)) %>% rep(., times = c(4^0 + 4^1 + 4^2)))) %>% 
  # now we need to shift the positions over in accordance with draw, like before
  mutate(denominator = ifelse(draw == 1, .5,
                              ifelse(draw == 2, .5 / 4,
                                     .5 / 4^2))) %>% 
  mutate(position = position - denominator) %>% 
  # here we'll add an index for which pie wedge we're working with
  mutate(pie_index = rep(letters[1:3], each = n()/3)) %>% 
  # to get the position axis correct for pie_index == "b" or "c", we'll need to offset
mutate(position = ifelse(pie_index == "a", position,
                         ifelse(pie_index == "b", position + 4,
                                position + 4 * 2)))

move_over <- function(position, index) {
  ifelse(
    index == "a", position,
    ifelse(
      index == "b", position + 4, position + 4 * 2
    )
  )
}

lines_1 <-
  tibble(x    = rep(1:4, each = 4) %>% rep(., times = 3),
         xend = ((1:4^2) / 4)      %>% rep(., times = 3),
         y    = 1,
         yend = 2) %>% 
  mutate(x    = x - .5,
         xend = xend - .5 / 4^1) %>% 
  # here we'll add an index for which pie wedge we're working with
  mutate(pie_index = rep(letters[1:3], each = n()/3)) %>% 
  # to get the position axis correct for `pie_index == "b"` or `"c"`, we'll need to offset
  mutate(x    = move_over(position = x,    index = pie_index),
         xend = move_over(position = xend, index = pie_index))


lines_2 <-
  tibble(x    = rep((1:4^2) / 4, each = 4) %>% rep(., times = 3),
         xend = (1:4^3 / 4^2)              %>% rep(., times = 3),
         y    = 2,
         yend = 3) %>% 
  mutate(x    = x - .5 / 4^1,
         xend = xend - .5 / 4^2) %>% 
  # here we'll add an index for which pie wedge we're working with
  mutate(pie_index = rep(letters[1:3], each = n()/3)) %>% 
  # to get the position axis correct for `pie_index == "b"` or `"c"`, we'll need to offset
  mutate(x    = move_over(position = x,    index = pie_index),
         xend = move_over(position = xend, index = pie_index))

d <- 
  d %>% 
  mutate(remain = c(# pie_index == "a"
    rep(0:1, times = c(1, 3)),
    rep(0,   times = 4),
    rep(1:0, times = c(1, 3)) %>% rep(., times = 3),
    rep(0,   times = 4 * 4),
    rep(c(0, 1, 0), times = c(1, 3, 4 * 3)) %>% rep(., times = 3),
    # pie_index == "b"
    rep(0:1, each = 2),
    rep(0,   times = 4 * 2),
    rep(1:0, each = 2) %>% rep(., times = 2),
    rep(0,   times = 4 * 4 * 2),
    rep(c(0, 1, 0, 1, 0), times = c(2, 2, 2, 2, 8)) %>% rep(., times = 2),
    # pie_index == "c",
    rep(0:1, times = c(3, 1)),
    rep(0,   times = 4 * 3),
    rep(1:0, times = c(3, 1)), 
    rep(0,   times = 4 * 4 * 3),
    rep(0:1, times = c(3, 1)) %>% rep(., times = 3),
    rep(0,   times = 4)
  )
  )

lines_1 <-
  lines_1 %>% 
  mutate(remain = c(rep(0,   times = 4),
                    rep(1:0, times = c(1, 3)) %>% rep(., times = 3),
                    rep(0,   times = 4 * 2),
                    rep(1:0, each  = 2) %>% rep(., times = 2),
                    rep(0,   times = 4 * 3),
                    rep(1:0, times = c(3, 1))
  )
  )

lines_2 <-
  lines_2 %>% 
  mutate(remain = c(rep(0,   times = 4 * 4),
                    rep(c(0, 1, 0), times = c(1, 3, 4 * 3)) %>% rep(., times = 3),
                    rep(0,   times = 4 * 8),
                    rep(c(0, 1, 0, 1, 0), times = c(2, 2, 2, 2, 8)) %>% rep(., times = 2),
                    rep(0,   times = 4 * 4 * 3),
                    rep(0:1, times = c(3, 1)) %>% rep(., times = 3),
                    rep(0,   times = 4)
  )
  )
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_vline(xintercept = c(0, 4, 8), color = "red", linewidth = 2/3) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               linewidth = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               linewidth = 1/3) +
  geom_point(aes(fill = fill, size = draw, alpha = remain %>% as.character()),
             shape = 21) +
  scale_fill_manual(values = c("navy", "white")) +
  scale_size_continuous(range = c(3, 1.5)) +
  scale_alpha_manual(values = c(0.2, 1)) +
  scale_x_continuous(NULL, limits = c(0, 12), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3.5), breaks = NULL) +
  coord_polar() +
  theme_cowplot(font_size = 20) +
  theme(legend.position = "none", 
        axis.line = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank())
```

]

.pull-right[
  
  ```{r, echo = F, results = 'asis'}
  # if we make two custom functions, here, 
  # it will simplify the `mutate()` code, below
  n_blue  <- function(x) rowSums(x == "b")
  n_white <- function(x) rowSums(x == "w")
  
  # make the data
  t <-
    tibble(d1 = rep(c("w", "b"), times = c(1, 4)),
           d2 = rep(c("w", "b"), times = c(2, 3)),
           d3 = rep(c("w", "b"), times = c(3, 2)),
           d4 = rep(c("w", "b"), times = c(4, 1))) %>% 
    mutate(blue1 = n_blue(.),
           white = n_white(.),
           blue2 = n_blue(.)) %>% 
    mutate(product = blue1 * white * blue2)
  
  # format the table
  t %>%
    transmute(conjecture = str_c("[", d1, " ", d2, " ", d3, " ", d4, "]"),
              `Ways to produce [w b w]` = str_c(blue1, " * ", white, " * ", blue2, " = ", product)) %>% 
    flextable() %>% 
    width(j = 1:2, width = c(1, 2)) %>% 
    align(align = "center", part = "all")
  ```
  
]

---
  
  ```{r, echo = F, results = 'asis'}
# format the table
t %>%
  transmute(conjecture = str_c("[", d1, " ", d2, " ", d3, " ", d4, "]"),
            `Ways to produce [w b w]` = str_c(blue1, " * ", white, " * ", blue2, " = ", product)) %>% 
  flextable() %>% 
  width(j = 1:2, width = c(1, 2)) %>% 
  align(align = "center", part = "all")
```

From these counts, we can see intuitively that the sequence is more likely to occur when there are 3 blue and 1 white marble, although also likely to occur when there are 2 of each. Of course, this assumes what?
  
  ???
  
  assumes that all the options are equally likely to begin with.

---