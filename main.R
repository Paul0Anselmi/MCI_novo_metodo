gpa <-
  structure(list(gpa = c(3.8900000000000001, 3.8999999999999999, 
                         3.75, 3.6000000000000001, 4, 3.1499999999999999, 3.25, 3.9249999999999998, 
                         3.4279999999999999, 3.7999999999999998, 3.8999999999999999, 2.8999999999999999, 
                         3.9249999999999998, 3.6499999999999999, 3.75, 4.6699999999999999, 
                         3.1000000000000001, 3.7999999999999998, 3.3999999999999999, 3.5750000000000002, 
                         3.8500000000000001, 3.3999999999999999, 3.5, 3.6000000000000001, 
                         3.8250000000000002, 3.9249999999999998, 4, 3.4249999999999998, 
                         3.75, 3.1499999999999999, 3.3999999999999999, 3.7000000000000002, 
                         3.3599999999999999, 3.7000000000000002, 3.7000000000000002, 3.6000000000000001, 
                         3.8250000000000002, 3.2000000000000002, 3.5, 3.5, 3, 3.98, 3.7000000000000002, 
                         3.8100000000000001, 4, 3.1000000000000001, 3.3999999999999999, 
                         3.5, 3.6499999999999999, 3.7000000000000002, 3.1000000000000001, 
                         4, 3.3500000000000001, 3.5409999999999999, 2.8999999999999999
  ), studyweek = c(50L, 15L, 15L, 10L, 25L, 20L, 15L, 10L, 12L, 
                   2L, 10L, 30L, 30L, 21L, 10L, 14L, 12L, 12L, 4L, 45L, 6L, 10L, 
                   12L, 13L, 35L, 10L, 40L, 14L, 30L, 8L, 8L, 20L, 40L, 15L, 25L, 
                   10L, 18L, 15L, 30L, 11L, 28L, 4L, 4L, 25L, 42L, 3L, 42L, 25L, 
                   20L, 7L, 6L, 20L, 45L, 30L, 20L), sleepnight = c(6, 6, 7, 6, 
                                                                    7, 7, 6, 8, 8, 8, 8, 6, 7, 9, 8.5, 6.5, 7.5, 8, 9, 6.5, 7, 7, 
                                                                    8, 6, 8, 8, 8, 9, 6, 6, 6.5, 7, 7, 7, 5, 7, 7, 6, 8, 7, 6, 7, 
                                                                    5, 7.5, 5, 7, 9, 8, 6, 8, 8, 7, 6, 7.5, 6), out = c(3, 1, 1, 
                                                                                                                        4, 3, 3, 1, 3, 2, 4, 1, 2, 2, 3, 3.5, 3, 3.5, 1, 3, 1.5, 2.5, 
                                                                                                                        3, 2, 3.5, 4, 3, 3, 3, 0, 0, 2, 1, 1, 1.5, 1, 2, 1.5, 1, 3, 1.5, 
                                                                                                                        1.5, 1.5, 1, 2.5, 1, 2, 2, 2, 2, 2, 1, 3, 2, 1.5, 3), gender = structure(c(1L, 
                                                                                                                                                                                                   1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
                                                                                                                                                                                                   1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                                                                                                                                                   2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 
                                                                                                                                                                                                   1L, 1L, 1L, 1L, 1L, 1L), .Label = c("female", "male"), class = "factor")), row.names = c(NA, 
                                                                                                                                                                                                                                                                                            -55L), class = c("tbl_df", "tbl", "data.frame"))


randomization_test <- function(data, var_response, var_group, n_sim = 1000, conf_level = 0.95) {
  # Calcular a diferença real das médias entre os grupos
  real_diff <- diff(tapply(data[[var_response]], data[[var_group]], mean))
  
  # Criar as diferenças simuladas por randomização
  randomized_diffs <- replicate(n_sim, {
    shuffled_group <- sample(data[[var_group]])  # Embaralhar os rótulos do grupo
    diff(tapply(data[[var_response]], shuffled_group, mean))
  })
  
  # Calcular o p-valor
  p_value <- mean(abs(randomized_diffs) >= abs(real_diff))
  
  # Calcular intervalo de confiança
  alpha <- 1 - conf_level
  ci <- quantile(randomized_diffs, probs = c(alpha / 2, 1 - alpha / 2))
  
  return(list(real_diff = real_diff, p_value = p_value, randomized_diffs = randomized_diffs, ci = ci))
}

# Aplicar o teste aos dados (comparando gpa entre gêneros)
result_randomization <- randomization_test(gpa, var_response = "gpa", var_group = "gender", n_sim = 1000)

# Exibir os resultados
cat("Diferença real entre as médias:", result_randomization$real_diff, "\n")
cat("p-valor do Teste de Randomização:", result_randomization$p_value, "\n")
cat("Intervalo de confiança (95%): [", result_randomization$ci[1], ", ", result_randomization$ci[2], "]\n")

hist(result_randomization$randomized_diffs, main = "Distribuição das Diferenças Simuladas",
     xlab = "Diferença Simulada", breaks = 30, col = "skyblue")
abline(v = result_randomization$real_diff, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Diferença Real"), col = c("red"), lty = 2, lwd = 2)

ggplot(gpa, aes(x = gender, y = gpa, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribuição de GPA por Gênero", x = "Gênero", y = "GPA") +
  theme_minimal()
