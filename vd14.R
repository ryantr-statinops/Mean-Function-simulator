# Môn học: Quá trình ngẫu nhiên - C03021
# Mô phỏng hàm trung bình (Mean function) - ví dụ 14
# Tài liệu: Chương 1 mục 5, trang 32/76

# 1. CÀI ĐẶT VÀ NẠP THƯ VIỆN ===================================================
required_packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra", "scales", "latex2exp", "grid")

for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# 2. THIẾT LẬP THAM SỐ MÔ PHỎNG ================================================
set.seed(03021) 

n_simulations <- 10000      # Số lượng mô phỏng Monte Carlo
n_max <- 30                 # Số năm tối đa
r_min <- 0.04               # Giá trị min của R
r_max <- 0.05               # Giá trị max của R
initial_amount <- 1000      # Số tiền ban đầu

# 3. HÀM TÍNH TOÁN LÝ THUYẾT ====================================================
calculate_theoretical_mean <- function(n_vector, r_min, r_max, initial = 1000) {
  theoretical_means <- sapply(n_vector, function(n) {
    if (n == 0) return(initial)
    integrand <- function(r) { (1 + r)^n * (1/(r_max - r_min)) }
    integral <- integrate(integrand, lower = r_min, upper = r_max)$value
    return(initial * integral)
  })
  return(theoretical_means)
}

calculate_theoretical_variance <- function(n_vector, r_min, r_max, initial = 1000) {
  theoretical_vars <- sapply(n_vector, function(n) {
    if (n == 0) return(0)
    e_xn <- calculate_theoretical_mean(n, r_min, r_max, initial)
    integrand_sq <- function(r) { (1 + r)^(2*n) * (1/(r_max - r_min)) }
    e_xn_sq <- initial^2 * integrate(integrand_sq, lower = r_min, upper = r_max)$value
    return(e_xn_sq - e_xn^2)
  })
  return(theoretical_vars)
}

calculate_theoretical_sd <- function(n_vector, r_min, r_max, initial = 1000) {
  variance <- calculate_theoretical_variance(n_vector, r_min, r_max, initial)
  return(sqrt(variance))
}

# 4. THỰC HIỆN MÔ PHỎNG MONTE CARLO ============================================
cat("Bắt đầu mô phỏng Monte Carlo...\n")
R_values <- runif(n_simulations, min = r_min, max = r_max)
X_matrix <- matrix(0, nrow = n_simulations, ncol = n_max + 1)

for (i in 1:n_simulations) {
  R <- R_values[i]
  X_matrix[i, ] <- initial_amount * (1 + R)^(0:n_max) # Tối ưu hóa vòng lặp
}

# 5. TÍNH TOÁN THỐNG KÊ MÔ PHỎNG ===============================================
n_vector <- 0:n_max
simulated_means <- colMeans(X_matrix)
theoretical_means <- calculate_theoretical_mean(n_vector, r_min, r_max, initial_amount)

conf_intervals <- t(apply(X_matrix, 2, function(x) {
  se <- sd(x) / sqrt(length(x))
  mean_val <- mean(x)
  c(mean_val - 1.96 * se, mean_val + 1.96 * se)
}))

summary_stats <- data.frame(
  n = n_vector,
  simulated_mean = simulated_means,
  theoretical_mean = theoretical_means,
  ci_lower = conf_intervals[, 1],
  ci_upper = conf_intervals[, 2],
  # SỬA LỖI: th eoretical_means -> theoretical_means
  relative_error = abs((simulated_means - theoretical_means) / theoretical_means) * 100
)

# 6. VẼ BIỂU ĐỒ CHÍNH =========================================================
main_plot <- ggplot(summary_stats, aes(x = n)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = simulated_mean, color = "Giá trị mô phỏng"), linewidth = 1.2) +
  geom_point(aes(y = simulated_mean), color = "darkblue", size = 2) +
  geom_line(aes(y = theoretical_mean, color = "Giá trị lý thuyết"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = theoretical_mean), color = "darkred", size = 1.8, shape = 17) +
  scale_color_manual(name = NULL, values = c("Giá trị mô phỏng" = "darkblue", "Giá trị lý thuyết" = "darkred")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", accuracy = 1), expand = expansion(mult = c(0.05, 0.1))) +
  labs(x = "Thời gian (n năm)", y = "Giá trị kỳ vọng E[X_n]") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.2, 0.85), 
        legend.background = element_rect(fill = "white", color = "gray80"))

# 7. BIỂU ĐỒ PHỤ ==============================================================
plot_r_distribution <- ggplot(data.frame(R = R_values), aes(x = R)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1) +
  labs(title = "Phân phối của biến R", x = "Lãi suất R", y = "Mật độ") +
  theme_minimal(base_size = 10)

plot_error <- ggplot(summary_stats, aes(x = n, y = relative_error)) +
  geom_line(color = "purple3", linewidth = 1) +
  labs(title = "Sai số tương đối", x = "Năm", y = "Sai số (%)") +
  theme_minimal(base_size = 10)

# 8. KẾT HỢP BIỂU ĐỒ ===========================================================
final_plot <- grid.arrange(
  main_plot, plot_r_distribution, plot_error,
  layout_matrix = rbind(c(1,1,2), c(1,1,3)),
  top = textGrob("MÔ PHỎNG QUÁ TRÌNH NGẪU NHIÊN - VÍ DỤ 14", gp = gpar(fontsize = 16, fontface = "bold"))
)

# 10. HIỂN THỊ KẾT QUẢ THỐNG KÊ ===============================================
cat("\n", strrep("=", 70), "\n")
cat("KẾT QUẢ MÔ PHỎNG - VÍ DỤ 14\n")
cat(strrep("=", 70), "\n\n")

cat("THAM SỐ MÔ PHỎNG:\n")
cat("  Số mô phỏng Monte Carlo:", format(n_simulations, big.mark = ","), "\n")
cat("  Số năm (n): từ 0 đến", n_max, "\n")
cat("  Phân phối R: U(", r_min, ", ", r_max, ")\n", sep = "")
cat("  Số tiền ban đầu: $", format(initial_amount, big.mark = ","), "\n\n", sep = "")

cat("KẾT QUẢ CHO MỘT SỐ GIÁ TRỊ n:\n")
cat(strrep("-", 70), "\n")
cat(sprintf("%5s %15s %15s %12s\n", "n", "Mô phỏng", "Lý thuyết", "Sai số (%)"))
cat(strrep("-", 70), "\n")

selected_n <- c(0, 1, 5, 10, 20, 30)
for (n in selected_n) {
  idx <- n + 1
  cat(sprintf("%5d %15.2f %15.2f %12.4f\n", 
              n, 
              summary_stats$simulated_mean[idx],
              summary_stats$theoretical_mean[idx],
              summary_stats$relative_error[idx]))
}

cat(strrep("=", 70), "\n")
cat("MÔ PHỎNG HOÀN TẤT!\n")
cat(strrep("=", 70), "\n")

