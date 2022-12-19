
library("tidyverse")
library("afex")
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom", 
                  panel.grid.major.x = element_blank()))

options(pillar.sigfig = 2)

data("fhch2010")
fhch <- as_tibble(droplevels(fhch2010[ fhch2010$correct,])) # remove errors
str(fhch2010) # structure of the data

####

ylab <- "response time (s)"

##---------------------------------------------------------------
##                        Aggregated Data                       -
##---------------------------------------------------------------

fhch %>% 
  group_by(task) %>% 
  summarise(m = mean(rt)) %>% 
  mutate(ov_mean = mean(m)) %>% 
  mutate(diff = ov_mean - m)


fhch_agg <- fhch %>% 
  group_by(id, task) %>% 
  summarise(rt = mean(rt))
lm1 <- lm(rt ~ task, fhch_agg)
fhch_agg$resid <- residuals(lm1)


##---------------------------------------------------------------
##                      Make Residuals Plot                     -
##---------------------------------------------------------------

##### make residuals figure

vio_width <- 0.9
point_width <- 0.2

set.seed(1234566) ## needed to ensure jitter is the same each time plot is created
p1 <- ggplot(fhch_agg, aes(x = task, y = rt)) +
  geom_violin(width = vio_width) +
  ggbeeswarm::geom_quasirandom(width = point_width) +
  stat_summary(colour = "red") +
  #coord_cartesian(ylim = c(0, 100)) +
  labs(y = ylab)
p1

set.seed(1234566)  ## needed to ensure jittered points are at same position
p1_de <- ggplot_build(p1)

lap2 <- fhch_agg %>% 
  mutate(xpoint = p1_de$data[[2]]$x)

lap3 <- fhch_agg %>% 
  group_by(task) %>% 
  summarise(rt = mean(rt))

lap4 <- lap3 %>% 
  summarise(rt = mean(rt))

lap5 <- p1_de$data[[1]] %>% 
  group_by(x) %>% 
  summarise(xmin = first(xmin),
            xmax = first(xmax)) %>% 
  bind_cols(lap3)

lap6 <- p1_de$data[[2]] %>% 
  group_by(group) %>% 
  mutate(rt = mean(y)) %>% 
  as_tibble() %>% 
  select(-size, - fill, -alpha, - stroke)

set.seed(1234566)
p2 <- ggplot(fhch_agg, aes(x = task, y = rt)) +
  geom_violin(width = vio_width) +
  geom_segment(data = lap6, 
               mapping = aes(x = x, xend = x, y = rt, yend = y),
               colour = "grey") +
  ggbeeswarm::geom_quasirandom(width = point_width) +
  geom_hline(yintercept = lap4$rt, linetype = 3, colour = "blue") +
  geom_segment(data = lap5, mapping = aes(x = xmin, xend = xmax,
                                          y = rt, yend = rt),
               colour = "red", linetype = 2) +
  stat_summary(colour = "red") +
  labs(y = ylab)
p2

cowplot::plot_grid(p1, p2)
ggsave("simple-residuals.pdf", device = "pdf", 
       width = 19, height = 8, units = "cm")


##----------------------------------------------------------------
##                        individual plots                       -
##----------------------------------------------------------------

fhch_indiv <- fhch %>% 
  filter(id %in% c("L1", "L5")) %>% 
  droplevels()

fhch_indiv %>% 
  ggplot(aes(x = id, y = rt)) +
  geom_violin(width = vio_width) +
  ggbeeswarm::geom_quasirandom(width = point_width) +
  #stat_summary(colour = "red") +
  #coord_cartesian(ylim = c(0, 100)) +
  labs(y = ylab)



##---------------------------------------------------------------
##                            table                             -
##---------------------------------------------------------------



fhch_agg %>% 
  print(n = Inf)

p0_a <- fhch_agg %>% 
  filter(id %in% c(paste0("N", 1:7)))
p0_b <- fhch_agg %>% 
  filter(id %in% c(paste0("L", 1:6)))

fhch2 <- fhch %>% 
  select(id, task, rt)
lm2 <- lm(rt ~ task, fhch2)
fhch2$resid_lm <- residuals(lm2)
mm2 <- mixed(rt ~ task + (1|id), fhch2, method = "S")
fhch2$resid_lmm <- residuals(mm2$full_model)
ranef(mm2$full_model)

p1 <- fhch2 %>% 
  group_by(id) %>% 
  mutate(trial = seq_len(n())) %>% 
  filter(id %in% c("L1")) %>% 
  filter((rt < 0.8 & trial < 4) | (rt > 0.8 & trial < 40)) %>% 
  select(-trial) %>% 
  slice(1:3, 6, 12)
p2 <- fhch2 %>% 
  group_by(id) %>% 
  mutate(trial = seq_len(n())) %>% 
  filter(id %in% c("L5")) %>% 
  filter((rt < 1.3 & trial < 10) | (rt > 1.3 & trial < 5)) %>% 
  select(-trial)

out <- bind_cols(bind_rows(p0_a, tibble(id = NA), p0_b), 
                 NA, 
                 bind_rows(p1, tibble(id = NA), p2))

print(xtable::xtable(out, digits = 1), include.rownames = FALSE)

####

mm <- mixed(rt ~ task*stimulus*length + (stimulus*length|id), fhch, method = "S")
mm

mm2 <- mixed(rt ~ task*stimulus*length + (stimulus*length||id), fhch, method = "S", expand_re = TRUE)
mm2

mm3 <- mixed(rt ~ task*stimulus*length + (stimulus+length||id), fhch, method = "S", expand_re = TRUE)
mm3

summary(mm3)

mm4 <- mixed(rt ~ task*stimulus*length + (stimulus||id), fhch, method = "S", expand_re = TRUE)
mm4
