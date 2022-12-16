ggplot(data = mn, aes(x = sat)) +
  stat_density(fill = "#f28e2b") +
  theme_light() +
  xlab("SAT") +
  ylab("Density") 


# ggplot(data = mn, aes(x = log2(sat))) +
#   stat_density(fill = "#4e79a7") +
#   theme_light() +
#   xlab("log2(SAT)") +
#   ylab("Density") 




data.frame(
  sat = c(mn$sat, log2(mn$sat)),
  type = c(rep("SAT", 33), rep("log2(SAT)", 33))
) %>%
ggplot(aes(x = sat, fill = type)) +
  stat_density() +
  theme_light() +
  xlab("") +
  xlim(0, 15) +
  ylab("Density") +
  ggthemes::scale_fill_tableau()



ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.3) +
  geom_function(
    fun = function(x) {-306.7 + 106.4 * log(x, base = 2)}
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")





ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.2, size = 4) +
  geom_function(
    fun = function(x) {-306.7 + 153.6*log(x)},
    color = "blue",
    linetype = "solid"
  ) +
  geom_function(
    fun = function(x) {-366.34 + 62.72*x - 2.15*x^2},
    color = "red",
    linetype = "dashed"
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



