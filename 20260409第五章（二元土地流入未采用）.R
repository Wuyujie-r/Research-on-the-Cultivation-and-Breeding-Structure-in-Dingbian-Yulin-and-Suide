# =============================================================================
# 第五章：陕北农牧交错区耕种-养殖结构类型及演变方向的影响因素分析
# 因变量：1) 静态类型（多项Logit）；2) 组合关系变化（二元Logit）；
#         3) 种植规模扩大/土地流入（二元Logit）
# =============================================================================

# ------------------------------ 1. 加载包 -------------------------------------
packages_needed <- c("tidyverse", "writexl", "nnet", "broom")
for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ------------------------------ 2. 路径设置 -----------------------------------
input_file <- "D:/本科毕业论文/20260409模型脚本结果/数据清洗结果/清洗后核心变量.xlsx"
output_dir <- "D:/本科毕业论文/20260409模型脚本结果/第五章结果"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------ 3. 读取数据并筛选农业经营户 -------------------
df <- read_xlsx(input_file)

df <- df %>%
  mutate(
    crop_share = ifelse(is.na(income_crop_share), 0, income_crop_share),
    livestock_share = ifelse(is.na(income_livestock_share), 0, income_livestock_share),
    ag_income_share = crop_share + livestock_share,
    ag_household = ifelse(
      ag_income_share >= 0.2 & (crop_share > 0 | livestock_share > 0 | 
                                  cultivated_area > 0 | livestock_su > 0),
      1, 0
    )
  ) %>%
  filter(ag_household == 1)

# 因变量：静态类型（若没有则计算）
if (!"static_type" %in% names(df)) {
  df <- df %>%
    mutate(
      share_diff = abs(crop_share - livestock_share),
      static_type = case_when(
        share_diff <= 0.1 ~ "种养结合型",
        crop_share > livestock_share ~ "种植主导型",
        livestock_share > crop_share ~ "养殖主导型",
        TRUE ~ NA_character_
      )
    )
}
df <- df %>%
  mutate(static_type = factor(static_type, levels = c("种植主导型", "养殖主导型", "种养结合型")))

# 因变量：兼营弱化/单一化强化（若没有则计算）
if (!"weaken_specialize" %in% names(df)) {
  df <- df %>%
    mutate(
      both_now = ifelse(crop_share > 0 & livestock_share > 0, 1, 0),
      relation_change = case_when(
        both_now == 1 & ag_income_share >= 0.6 ~ "兼营增强型",
        both_now == 0 & ag_income_share < 0.4 ~ "兼营弱化型",
        both_now == 0 & (crop_share > 0.7 | livestock_share > 0.7) ~ "单一化强化型",
        TRUE ~ "结构保持稳定型"
      ),
      weaken_specialize = ifelse(relation_change %in% c("兼营弱化型", "单一化强化型"), 1, 0)
    )
}

# 因变量：种植规模扩大（土地流入）
df <- df %>%
  mutate(
    land_inflow = ifelse(is.na(land_inflow), 0, land_inflow)
  )

# ------------------------------ 4. 构造自变量 ---------------------------------
df_model <- df %>%
  mutate(
    # 资源禀赋
    ln_cultivated = log(cultivated_area + 0.1),
    ln_grassland = log(grassland_area + 0.1),
    ln_livestock = log(livestock_su + 0.1),
    irrigation = ifelse(is.na(irrigation), 0, irrigation),
    
    # 家庭经营
    labor_num = ifelse(is.na(labor_num), 0, labor_num),
    long_migrant = ifelse(is.na(long_migrant), 0, long_migrant),
    ag_income_ratio = ag_income_share,
    off_farm_ratio = ifelse(is.na(income_offfarm_share), 0, income_offfarm_share) +
      ifelse(is.na(income_subsidy_share), 0, income_subsidy_share),
    
    # 风险认知
    income_unstable = ifelse(is.na(income_unstable), 0, income_unstable),
    risk_encounter = ifelse(is.na(risk_encounter), 0, risk_encounter),
    
    # 外部支持
    policy_support = ifelse(is.na(policy_support), 0, policy_support),
    tech_training = ifelse(is.na(tech_training), 0, tech_training),
    coop_member = ifelse(is.na(coop_member), 0, coop_member),
    infra_improve = irrigation,
    
    # 县域情境
    county_dingbian = ifelse(county == "定边县", 1, 0),
    county_yuyang = ifelse(county == "榆阳区", 1, 0)
  ) %>%
  select(static_type, weaken_specialize, land_inflow,
         ln_cultivated, ln_grassland, ln_livestock, irrigation,
         labor_num, long_migrant, ag_income_ratio, off_farm_ratio,
         income_unstable, risk_encounter,
         policy_support, tech_training, coop_member, infra_improve,
         county_dingbian, county_yuyang) %>%
  drop_na()

cat("回归分析有效样本量：", nrow(df_model), "\n")
cat("种植规模扩大（land_inflow）分布：\n")
print(table(df_model$land_inflow))

# ------------------------------ 5. 多项Logit模型（静态类型）-------------------
mlogit_model <- multinom(static_type ~ ln_cultivated + ln_grassland + ln_livestock + 
                           irrigation + labor_num + long_migrant + 
                           ag_income_ratio + off_farm_ratio +
                           income_unstable + risk_encounter +
                           policy_support + tech_training + coop_member + infra_improve +
                           county_dingbian + county_yuyang,
                         data = df_model, trace = FALSE)

summary_mlogit <- summary(mlogit_model)
coef_mlogit <- coef(mlogit_model)
se_mlogit <- summary_mlogit$standard.errors
z_mlogit <- coef_mlogit / se_mlogit
p_mlogit <- 2 * (1 - pnorm(abs(z_mlogit)))

results_mlogit <- list()
for (i in 1:nrow(coef_mlogit)) {
  type <- rownames(coef_mlogit)[i]
  df_temp <- data.frame(
    变量 = colnames(coef_mlogit),
    系数 = coef_mlogit[i, ],
    标准误 = se_mlogit[i, ],
    z值 = z_mlogit[i, ],
    p值 = p_mlogit[i, ],
    RRR = exp(coef_mlogit[i, ])
  )
  df_temp$显著性 <- ifelse(df_temp$p值 < 0.001, "***",
                        ifelse(df_temp$p值 < 0.01, "**",
                               ifelse(df_temp$p值 < 0.05, "*", "")))
  results_mlogit[[type]] <- df_temp
}
write_xlsx(results_mlogit[["养殖主导型"]], file.path(output_dir, "表5-2_养殖主导型_vs_种植主导型.xlsx"))
write_xlsx(results_mlogit[["种养结合型"]], file.path(output_dir, "表5-2_种养结合型_vs_种植主导型.xlsx"))

# ------------------------------ 6. 二元Logit模型 --------------------------------
run_logit <- function(formula, data, model_name) {
  model <- glm(formula, data = data, family = binomial)
  coef_tab <- coef(summary(model))
  OR <- exp(coef_tab[, "Estimate"])
  res <- data.frame(
    变量 = rownames(coef_tab),
    系数 = coef_tab[, "Estimate"],
    标准误 = coef_tab[, "Std. Error"],
    z值 = coef_tab[, "z value"],
    p值 = coef_tab[, "Pr(>|z|)"],
    优势比_OR = OR,
    显著性 = ifelse(coef_tab[, "Pr(>|z|)"] < 0.001, "***",
                 ifelse(coef_tab[, "Pr(>|z|)"] < 0.01, "**",
                        ifelse(coef_tab[, "Pr(>|z|)"] < 0.05, "*", "")))
  )
  write_xlsx(res, file.path(output_dir, paste0("表5-3_", model_name, ".xlsx")))
  return(model)
}

formula_base <- as.formula("~ ln_cultivated + ln_grassland + ln_livestock + 
                           irrigation + labor_num + long_migrant + 
                           ag_income_ratio + off_farm_ratio +
                           income_unstable + risk_encounter +
                           policy_support + tech_training + coop_member + infra_improve +
                           county_dingbian + county_yuyang")

# 模型1：组合关系变化（兼营弱化/单一化强化）
model_weak <- run_logit(update(formula_base, weaken_specialize ~ .), df_model, "组合关系变化_单一化方向")

# 模型2：种植规模扩大（土地流入）
# 注意：如果 land_inflow 中 1 的数量太少（如<10），模型可能无法收敛。此时建议跳过或改用其他变量。
if (sum(df_model$land_inflow) >= 5) {
  model_inflow <- run_logit(update(formula_base, land_inflow ~ .), df_model, "种植规模扩大")
} else {
  cat("警告：种植规模扩大（land_inflow）阳性样本仅", sum(df_model$land_inflow), "个，跳过模型估计。\n")
}

# ------------------------------ 7. 保存模型摘要 ---------------------------------
sink(file.path(output_dir, "模型估计摘要.txt"))
cat("========== 多项Logit模型（静态类型） ==========\n")
print(summary_mlogit)
cat("\n\n========== 二元Logit模型：组合关系变化（单一化方向） ==========\n")
print(summary(model_weak))
if (exists("model_inflow")) {
  cat("\n\n========== 二元Logit模型：种植规模扩大 ==========\n")
  print(summary(model_inflow))
}
sink()

cat("\n第五章分析完成！结果已保存至：", output_dir, "\n")

