##################################################
### Examine different chi-square PDFs
##################################################

fig_01 = data.frame(
  X = seq(from = 0, to = 10, by = 0.01)
) %>% 
  mutate(
    Y = dchisq(x = X, df = 1)
  )

fig_02 = data.frame(
  X = seq(from = 0, to = 10, by = 0.01)
) %>% 
  mutate(
    Y = dchisq(x = X, df = 3)
  )

fig_03 = data.frame(
  X = seq(from = 0, to = 10, by = 0.01)
) %>% 
  mutate(
    Y = dchisq(x = X, df = 5)
  )

# F-dist
fig_04 = data.frame(
  X = seq(from = 0, to = 10, by = 0.01)
) %>% 
  mutate(
    Y = df(x = X, df1 = 3, df2 = 5)
  )

# Show F is ratio of two chi-squared dist
fig_05 = data.frame(
  X = seq(from = 0, to = 10, by = 0.01)
) %>% 
  mutate(
    Y = fig_02$Y / fig_03$Y
  )

# View data
head(fig_01)


# Create plot
ggplot(data = fig_01, aes(x = X, y = Y)) +
  geom_line() +                                                    # X(1)
  # geom_line(data = fig_02, color = "orange") +                   # X(3)
  # geom_line(data = fig_03, color = "blue") +                     # X(5)
  #geom_line(data = fig_04, color = "red", linetype = "dashed") +  # F(3, 5)
  #geom_line(data = fig_04, color = "purple") +                    # X(3) / X(5)
  xlab(expression(chi^2)) +
  scale_y_continuous(name = "Probability density", limits = c(0, 0.5)) +
  theme_light() 
