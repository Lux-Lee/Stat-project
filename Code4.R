Health_Data  <- bind_rows(
  mutate(Indigenous_Health, group = "Indigenous"),
  mutate(Canadian_Health, group = "Canadian")
)

md <- data %>%                          ## Data formatting and factoring
  filter(Gender != "Both") %>%
  mutate(
    Percentage = Percentage / 100,
    Year = factor(Year),
    Gender = factor(Gender, levels = c("Women", "Men")),
    Group = factor(Group, levels = c("Canadian", "Indigenous")),
    Category = factor(Category, levels = c("Fair", "Good", "Excellent"), ordered = TRUE)
  )
