# =============================================================================
# 第六章：陕北农牧交错区农户经营适应行为分析
# 基于清洗后核心变量（农业经营户，N=248）
# 输出目录：D:/本科毕业论文/20260409模型脚本结果/第六章结果/
# =============================================================================

# ------------------------------ 1. 加载包 -------------------------------------
packages_needed <- c("tidyverse", "writexl", "openxlsx", "ggplot2", "nnet")
for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ------------------------------ 2. 路径设置 -----------------------------------
input_file <- "D:/本科毕业论文/20260409模型脚本结果/数据清洗结果/清洗后核心变量.xlsx"
output_dir <- "D:/本科毕业论文/20260409模型脚本结果/第六章结果"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------ 3. 读取数据并筛选农业经营户 -------------------
df <- read_xlsx(input_file)

# 收入占比处理（与第四章一致）
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
  filter(ag_household == 1)   # 248户

# ------------------------------ 4. 构造静态类型和演变特征（与第四章一致）-------
# 静态类型
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
df$static_type <- factor(df$static_type, levels = c("种植主导型", "养殖主导型", "种养结合型"))

# 演变特征：组合关系变化（单一化方向）
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
df$evolution_type <- ifelse(df$weaken_specialize == 1, "单一化方向", "其他")

# ------------------------------ 5. 构造适应行为因变量 -------------------------
# 根据论文6.1.1分类，结合问卷实际变量定义：

# 经营配置调整行为：是否改变主导产业 OR 是否有扩大种植/养殖意愿（但意愿属于未来，这里用实际行为）
# 可用变量：changed_industry（是否改变主导产业）
df$config_adjust <- ifelse(is.na(df$changed_industry), 0, df$changed_industry)

# 要素调适行为：土地流转（流入或流出） OR 是否调整劳动力投入（问卷无直接问题，用常年外出务工代理？）
# 我们使用土地流转（land_inflow==1 or land_outflow==1）作为要素调适指标
df$factor_adjust <- ifelse((!is.na(df$land_inflow) & df$land_inflow == 1) |
                             (!is.na(df$land_outflow) & df$land_outflow == 1), 1, 0)

# 外部支持利用行为：合作社参与 OR 技术培训 OR 灌溉设施（任意一项为1）
df$external_support <- ifelse(
  (!is.na(df$coop_member) & df$coop_member == 1) |
    (!is.na(df$tech_training) & df$tech_training == 1) |
    (!is.na(df$irrigation) & df$irrigation == 1), 1, 0
)

# 注意：原始数据中policy_support也可作为外部支持，但这里先按三类行为定义。
# 检查变量缺失并填充0
df <- df %>%
  mutate(
    config_adjust = ifelse(is.na(config_adjust), 0, config_adjust),
    factor_adjust = ifelse(is.na(factor_adjust), 0, factor_adjust),
    external_support = ifelse(is.na(external_support), 0, external_support)
  )

# ------------------------------ 6. 表6-1：各类行为采纳比例 --------------------
behavior_summary <- df %>%
  summarise(
    `经营配置调整` = sum(config_adjust),
    `要素调适` = sum(factor_adjust),
    `外部支持利用` = sum(external_support)
  ) %>%
  pivot_longer(everything(), names_to = "行为类别", values_to = "采纳户数") %>%
  mutate(
    采纳比例 = round(采纳户数 / nrow(df) * 100, 1),
    总户数 = nrow(df)
  ) %>%
  select(行为类别, 采纳户数, 采纳比例)
write_xlsx(behavior_summary, file.path(output_dir, "表6-1_适应行为采纳比例.xlsx"))

# ------------------------------ 7. 表6-2：行为组合特征 -----------------------
# 计算每个农户采纳了几类行为（0-3）
df <- df %>%
  mutate(behavior_count = config_adjust + factor_adjust + external_support)
combo_dist <- df %>%
  group_by(behavior_count) %>%
  summarise(户数 = n()) %>%
  mutate(占比 = round(户数 / nrow(df) * 100, 1))
write_xlsx(combo_dist, file.path(output_dir, "表6-2_行为组合分布.xlsx"))

# ------------------------------ 8. 表6-3：不同静态类型的适应行为采纳比例 ----
static_behavior <- df %>%
  group_by(static_type) %>%
  summarise(
    经营配置调整 = round(mean(config_adjust) * 100, 1),
    要素调适 = round(mean(factor_adjust) * 100, 1),
    外部支持利用 = round(mean(external_support) * 100, 1),
    样本数 = n()
  )
write_xlsx(static_behavior, file.path(output_dir, "表6-3_静态类型适应行为差异.xlsx"))

# ------------------------------ 9. 表6-4：不同演变特征的适应行为采纳比例 ----
# 按是否单一化方向分组
evolution_behavior <- df %>%
  group_by(evolution_type) %>%
  summarise(
    经营配置调整 = round(mean(config_adjust) * 100, 1),
    要素调适 = round(mean(factor_adjust) * 100, 1),
    外部支持利用 = round(mean(external_support) * 100, 1),
    样本数 = n()
  )
write_xlsx(evolution_behavior, file.path(output_dir, "表6-4_演变特征适应行为差异.xlsx"))

# ------------------------------ 10. 表6-5：三县适应行为采纳比例 -------------
county_behavior <- df %>%
  group_by(county) %>%
  summarise(
    经营配置调整 = round(mean(config_adjust) * 100, 1),
    要素调适 = round(mean(factor_adjust) * 100, 1),
    外部支持利用 = round(mean(external_support) * 100, 1),
    样本数 = n()
  )
write_xlsx(county_behavior, file.path(output_dir, "表6-5_县域适应行为差异.xlsx"))

# 可视化：县域适应行为比较图
p_county <- df %>%
  select(county, config_adjust, factor_adjust, external_support) %>%
  pivot_longer(cols = -county, names_to = "行为", values_to = "采纳") %>%
  group_by(county, 行为) %>%
  summarise(比例 = mean(采纳) * 100, .groups = "drop") %>%
  ggplot(aes(x = county, y = 比例, fill = 行为)) +
  geom_col(position = "dodge") +
  labs(x = "县域", y = "采纳比例 (%)", fill = "适应行为类别") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(output_dir, "图_县域适应行为比较.png"), p_county, width = 8, height = 5)

# ------------------------------ 11. 适应行为影响因素分析（二元Logit）--------
# 构造自变量（与第五章一致）
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
    
    # 外部支持（注意：这里的变量可能与因变量重叠，但作为自变量时不包括因变量本身）
    policy_support = ifelse(is.na(policy_support), 0, policy_support),
    tech_training = ifelse(is.na(tech_training), 0, tech_training),
    coop_member = ifelse(is.na(coop_member), 0, coop_member),
    infra_improve = irrigation,
    
    # 县域情境
    county_dingbian = ifelse(county == "定边县", 1, 0),
    county_yuyang = ifelse(county == "榆阳区", 1, 0)
  ) %>%
  select(config_adjust, factor_adjust, external_support,
         ln_cultivated, ln_grassland, ln_livestock, irrigation,
         labor_num, long_migrant, ag_income_ratio, off_farm_ratio,
         income_unstable, risk_encounter,
         policy_support, tech_training, coop_member, infra_improve,
         county_dingbian, county_yuyang) %>%
  drop_na()

cat("影响因素分析有效样本量：", nrow(df_model), "\n")

# 定义运行Logit的函数
run_logit <- function(dv, data, model_name) {
  formula <- as.formula(paste(dv, "~ ln_cultivated + ln_grassland + ln_livestock + 
                               irrigation + labor_num + long_migrant + 
                               ag_income_ratio + off_farm_ratio +
                               income_unstable + risk_encounter +
                               policy_support + tech_training + coop_member + infra_improve +
                               county_dingbian + county_yuyang"))
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
  write_xlsx(res, file.path(output_dir, paste0("表6-6_", model_name, ".xlsx")))
  return(model)
}

# 分别对三类行为建模
if (sum(df_model$config_adjust) >= 5) {
  model_config <- run_logit("config_adjust", df_model, "经营配置调整")
} else {
  cat("经营配置调整阳性样本过少，跳过模型\n")
}
if (sum(df_model$factor_adjust) >= 5) {
  model_factor <- run_logit("factor_adjust", df_model, "要素调适")
} else {
  cat("要素调适阳性样本过少，跳过模型\n")
}
if (sum(df_model$external_support) >= 5) {
  model_external <- run_logit("external_support", df_model, "外部支持利用")
} else {
  cat("外部支持利用阳性样本过少，跳过模型\n")
}

# ------------------------------ 12. 保存模型摘要 -----------------------------
sink(file.path(output_dir, "适应行为模型摘要.txt"))
if (exists("model_config")) {
  cat("========== 经营配置调整模型 ==========\n")
  print(summary(model_config))
}
if (exists("model_factor")) {
  cat("\n========== 要素调适模型 ==========\n")
  print(summary(model_factor))
}
if (exists("model_external")) {
  cat("\n========== 外部支持利用模型 ==========\n")
  print(summary(model_external))
}
sink()

cat("\n第六章分析完成！结果已保存至：", output_dir, "\n")
