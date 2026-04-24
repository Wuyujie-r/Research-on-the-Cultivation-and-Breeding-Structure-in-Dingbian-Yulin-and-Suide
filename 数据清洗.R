# =============================================================================
# 陕北农牧交错区耕种-养殖结构演变与农户经营适应行为研究
# 原始调查问卷数据清洗脚本（修正版 - 基于实际列名）
# 功能：读取原始Excel数据，清洗并计算核心变量，输出清洗结果
# 输出目录：D:\本科毕业论文\20260409模型脚本结果\数据清洗结果\
# 输出文件：
#   1. 清洗后完整数据.xlsx           - 清洗后的全量数据（含所有原始列和计算列）
#   2. 清洗后核心变量.xlsx            - 仅包含论文分析所需的核心变量
#   3. 变量映射表.xlsx                - 变量英文名、中文名、来源、类型等
#   4. 清洗后所有列名.txt              - 清洗后所有列名列表（已由清洗步骤生成）
#   5. 原列名与清洗后列名对照.txt      - 原列名与清洗后列名对照表（已由清洗步骤生成）
#   6. 数据清洗报告.txt                - 描述性统计和质量检查报告
# =============================================================================

# ------------------------------ 1. 加载必要的包 ------------------------------
packages_needed <- c("readxl", "dplyr", "stringr", "writexl", "fs", "tidyr", "purrr")
for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 记录开始时间
start_time <- Sys.time()

# ------------------------------ 2. 设置路径 -----------------------------------
input_path <- "D:/本科毕业论文/20260307调查问卷初始数据.xlsx"   # 请确保此路径正确
output_dir <- "D:/本科毕业论文/20260409模型脚本结果/数据清洗结果"

# 创建输出目录（如果不存在）
if (!dir_exists(output_dir)) {
  dir_create(output_dir, recursive = TRUE)
}

# ------------------------------ 3. 读取原始数据 ------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("步骤1：读取原始数据\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

raw_data <- read_excel(input_path, col_names = TRUE, guess_max = 10000)
cat("原始数据维度：", nrow(raw_data), "行 x", ncol(raw_data), "列\n")

# 创建数据副本用于清洗
df <- raw_data

# ------------------------------ 4. 清洗列名 -----------------------------------
cat("\n步骤2：清洗列名\n")

# 原始列名
orig_names <- colnames(df)

# 定义列名清洗函数
clean_colname <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)                                    # 去除前后空格
  x <- str_replace_all(x, "[[:space:]]+", "_")        # 空白字符替换为下划线
  x <- str_replace_all(x, "[（）()%*?/]", "_")         # 常见符号替换为下划线
  x <- str_replace_all(x, "_+", "_")                  # 合并连续下划线
  x <- str_replace_all(x, "^_+|_+$", "")              # 去除首尾下划线
  if (x == "") x <- "col"                              # 若为空则用默认名
  return(x)
}

# 应用清洗函数
new_names <- sapply(orig_names, clean_colname, USE.NAMES = FALSE)

# 处理重复列名
dup_check <- table(new_names)
if (any(dup_check > 1)) {
  cat("发现重复的列名，正在添加数字后缀...\n")
  for (nm in names(dup_check[dup_check > 1])) {
    idx <- which(new_names == nm)
    new_names[idx] <- paste0(nm, "_", seq_along(idx))
  }
}

# 将清洗后的列名赋予数据框
colnames(df) <- new_names

# 生成列名对照表
colname_map <- data.frame(
  原列名 = orig_names,
  新列名 = new_names,
  stringsAsFactors = FALSE
)

# 保存列名对照表
write.table(colname_map, file.path(output_dir, "原列名与清洗后列名对照.txt"),
            sep = "\t", row.names = FALSE, quote = FALSE)
writeLines(new_names, file.path(output_dir, "清洗后所有列名.txt"))

cat("列名清洗完成，共处理", ncol(df), "列\n")

# ------------------------------ 5. 处理缺失值和特殊值 -------------------------
cat("\n步骤3：处理缺失值和特殊值\n")

special_values <- c("(空)", "(跳过)", "未养殖", "不知道", "不了解", "无", "未考虑",
                    "视情况而定", "不清楚", "无意见", "其他", "无直接关联", "不确定",
                    "〖跳过〗", "(空)", "NULL", "NA", "")

for (col in names(df)) {
  if (is.character(df[[col]])) {
    df[[col]] <- ifelse(df[[col]] %in% special_values, NA, df[[col]])
  }
}

cat("特殊值已替换为NA\n")

# ------------------------------ 6. 提取和计算核心变量 -------------------------
cat("\n步骤4：提取和计算核心变量\n")

# 辅助函数：安全转换为数值
safe_as_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.character(x)) {
    x <- str_trim(x)
    x[x %in% c("", "(空)", "NULL", "NA")] <- NA
    return(suppressWarnings(as.numeric(x)))
  }
  return(rep(NA, length(x)))
}

# ==================== 6.1 基础人口学变量 ====================
cat("  - 基础人口学变量\n")

# 性别
gender_col <- "1.1_受访人性别："
if (gender_col %in% names(df)) {
  df$gender <- ifelse(grepl("男", df[[gender_col]]), 1, 0)
} else {
  df$gender <- NA
}

# 年龄
age_col <- "1_1.2年龄:_岁"
if (age_col %in% names(df)) {
  df$age <- safe_as_numeric(df[[age_col]])
} else {
  df$age <- NA
}

# 家庭常住人口
family_size_col <- "1_3.家庭常住人口数:"
if (family_size_col %in% names(df)) {
  df$family_size <- safe_as_numeric(df[[family_size_col]])
} else {
  df$family_size <- NA
}

# 劳动力数量
labor_col <- "1_4._家庭中实际劳动力数_15-64岁_:"
if (labor_col %in% names(df)) {
  df$labor_num <- safe_as_numeric(df[[labor_col]])
} else {
  df$labor_num <- NA
}

# 教育水平
edu_col <- "2.1_文化程度:"
if (edu_col %in% names(df)) {
  edu_raw <- df[[edu_col]]
  edu_map <- c("文盲" = 1, "小学" = 2, "初中" = 3, "高中" = 4,
               "大专" = 5, "本科" = 6, "硕士" = 7, "博士" = 8)
  df$edu_level <- edu_map[edu_raw]
  df$edu_level[is.na(df$edu_level)] <- 0  # 未知编码为0
} else {
  df$edu_level <- NA
}

# ==================== 6.2 区域变量 ====================
cat("  - 区域变量\n")

county_col <- "所属区县"
if (county_col %in% names(df)) {
  df$county <- df[[county_col]]
  df$county_dingbian <- ifelse(grepl("定边", df$county), 1, 0)
  df$county_yuyang   <- ifelse(grepl("榆阳", df$county), 1, 0)
  df$county_suide    <- ifelse(grepl("绥德", df$county), 1, 0)
} else {
  df$county <- NA
  df$county_dingbian <- df$county_yuyang <- df$county_suide <- NA
}

village_col <- "2_乡_镇_村"
df$village <- if (village_col %in% names(df)) df[[village_col]] else NA

# ==================== 6.3 收入变量 ====================
cat("  - 收入变量\n")

# 家庭总收入（万元转元）
income_total_col <- "1_5.1._家庭年总收入约:_万元"
if (income_total_col %in% names(df)) {
  df$income_total <- safe_as_numeric(df[[income_total_col]]) * 10000
} else {
  warning("未找到家庭总收入列")
  df$income_total <- NA
}

# 种植收入占比
crop_share_col <- "1_21._各类收入约占家庭年总收入的比例_估算_:种植"
if (crop_share_col %in% names(df)) {
  df$income_crop_share <- safe_as_numeric(df[[crop_share_col]]) / 100
  df$income_crop_share[df$income_crop_share < 0] <- 0
  df$income_crop_share[df$income_crop_share > 1] <- 1
} else {
  warning("未找到种植收入占比列")
  df$income_crop_share <- NA
}

# 畜牧收入占比
livestock_share_col <- "2_；_畜牧"
if (livestock_share_col %in% names(df)) {
  df$income_livestock_share <- safe_as_numeric(df[[livestock_share_col]]) / 100
  df$income_livestock_share[df$income_livestock_share < 0] <- 0
  df$income_livestock_share[df$income_livestock_share > 1] <- 1
} else {
  warning("未找到畜牧收入占比列")
  df$income_livestock_share <- NA
}

# 务工收入占比
offfarm_share_col <- "3_；务工"
if (offfarm_share_col %in% names(df)) {
  df$income_offfarm_share <- safe_as_numeric(df[[offfarm_share_col]]) / 100
  df$income_offfarm_share[df$income_offfarm_share < 0] <- 0
  df$income_offfarm_share[df$income_offfarm_share > 1] <- 1
} else {
  df$income_offfarm_share <- NA
}

# 补贴收入占比
subsidy_share_col <- "4_；政策补贴"
if (subsidy_share_col %in% names(df)) {
  df$income_subsidy_share <- safe_as_numeric(df[[subsidy_share_col]]) / 100
  df$income_subsidy_share[df$income_subsidy_share < 0] <- 0
  df$income_subsidy_share[df$income_subsidy_share > 1] <- 1
} else {
  df$income_subsidy_share <- NA
}

# ==================== 6.4 土地变量 ====================
cat("  - 土地变量\n")

# 耕地面积
cultivated_area_col <- "9.当前家庭土地情况—耕地_面积_亩"
if (cultivated_area_col %in% names(df)) {
  df$cultivated_area <- safe_as_numeric(df[[cultivated_area_col]])
  df$cultivated_area[is.na(df$cultivated_area)] <- 0
} else {
  df$cultivated_area <- 0
}

# 草地面积
grassland_area_col <- "草地_面积_亩"
if (grassland_area_col %in% names(df)) {
  df$grassland_area <- safe_as_numeric(df[[grassland_area_col]])
  df$grassland_area[is.na(df$grassland_area)] <- 0
} else {
  df$grassland_area <- 0
}

# 总土地面积
df$total_land <- df$cultivated_area + df$grassland_area
df$total_land[df$total_land == 0] <- NA  # 无土地设为NA，避免除零

# 耕地撂荒面积
abandon_crop_col <- "耕地_撂荒面积_亩"
if (abandon_crop_col %in% names(df)) {
  abandon_val <- safe_as_numeric(df[[abandon_crop_col]])
  df$abandoned_area <- ifelse(!is.na(abandon_val) & abandon_val > 0, abandon_val, 0)
} else {
  df$abandoned_area <- 0
}

# ==================== 6.5 牲畜变量 ====================
cat("  - 牲畜变量\n")

# 牛数量
cattle_col <- "29._当前养殖牲畜种类与数量:—牛_数量_头_只"
if (cattle_col %in% names(df)) {
  df$cattle_num <- safe_as_numeric(df[[cattle_col]])
} else {
  df$cattle_num <- 0
}

# 羊数量
sheep_col <- "羊_数量_头_只"
if (sheep_col %in% names(df)) {
  df$sheep_num <- safe_as_numeric(df[[sheep_col]])
} else {
  df$sheep_num <- 0
}

# 猪数量
pig_col <- "猪_数量_头_只"
if (pig_col %in% names(df)) {
  df$pig_num <- safe_as_numeric(df[[pig_col]])
} else {
  df$pig_num <- 0
}

# 鸡鸭数量
chicken_col <- "鸡_鸭等_数量_头_只"
if (chicken_col %in% names(df)) {
  df$chicken_num <- safe_as_numeric(df[[chicken_col]])
} else {
  df$chicken_num <- 0
}

# 将NA替换为0
df$cattle_num[is.na(df$cattle_num)] <- 0
df$sheep_num[is.na(df$sheep_num)] <- 0
df$pig_num[is.na(df$pig_num)] <- 0
df$chicken_num[is.na(df$chicken_num)] <- 0

# 计算标准羊单位（SU）：牛5，羊1，猪1.5，鸡0.02（参考常用换算）
df$livestock_su <- df$cattle_num * 5 + df$sheep_num * 1 + df$pig_num * 1.5 + df$chicken_num * 0.02

# 畜地比
df$stocking_density <- ifelse(df$total_land > 0, df$livestock_su / df$total_land, 0)

# ==================== 6.6 作物与饲草 ====================
cat("  - 作物与饲草\n")

# 查找所有作物名称列和面积列（清洗后列名格式固定）
crop_name_cols <- grep("_作物名称$", names(df), value = TRUE)
crop_area_cols <- grep("_种植面积_亩$", names(df), value = TRUE)

# 作物种类数
if (length(crop_name_cols) > 0) {
  df$crop_diversity <- apply(df[crop_name_cols], 1, function(row) {
    sum(!is.na(row) & !row %in% c("(空)", "", "无"))
  })
} else {
  df$crop_diversity <- 0
}

# 玉米面积（作为饲草代表）
corn_area <- 0
if (length(crop_name_cols) > 0 && length(crop_area_cols) > 0) {
  for (i in seq_along(crop_name_cols)) {
    if (i <= length(crop_area_cols)) {
      is_corn <- grepl("玉米", df[[crop_name_cols[i]]], ignore.case = TRUE)
      area_val <- safe_as_numeric(df[[crop_area_cols[i]]])
      area_val[is.na(area_val)] <- 0
      corn_area <- corn_area + ifelse(is_corn, area_val, 0)
    }
  }
}
df$corn_area <- corn_area
df$fodder_ratio <- ifelse(df$cultivated_area > 0, df$corn_area / df$cultivated_area, 0)
df$fodder_ratio[df$fodder_ratio > 1] <- 1

# 饲草自给率（近似：玉米面积 / (养殖SU * 0.5)，假设每SU需0.5亩饲草）
df$fodder_self_sufficiency <- ifelse(df$livestock_su > 0,
                                     df$corn_area / (df$livestock_su * 0.5),
                                     NA)
df$fodder_self_sufficiency[df$fodder_self_sufficiency > 5] <- 5  # 截尾

# ==================== 6.7 适应行为变量 ====================
cat("  - 适应行为变量\n")

# 近五年是否改变主导产业
change_industry_col <- "47.近五年是否改变主导产业？"
if (change_industry_col %in% names(df)) {
  df$changed_industry <- ifelse(grepl("是", df[[change_industry_col]]), 1, 0)
} else {
  df$changed_industry <- NA
}

# 土地流转：转入（耕地+草地）
inflow_crop_col <- "耕地_转入面积_亩"
inflow_grass_col <- "草地_转入面积_亩"
inflow_val <- rep(0, nrow(df))
if (inflow_crop_col %in% names(df)) {
  inflow_val <- inflow_val + (safe_as_numeric(df[[inflow_crop_col]]) > 0)
}
if (inflow_grass_col %in% names(df)) {
  inflow_val <- inflow_val + (safe_as_numeric(df[[inflow_grass_col]]) > 0)
}
df$land_inflow <- as.numeric(inflow_val > 0)

# 土地流转：转出（耕地+草地）
outflow_crop_col <- "耕地_转出面积_亩"
outflow_grass_col <- "草地_转出面积_亩"
outflow_val <- rep(0, nrow(df))
if (outflow_crop_col %in% names(df)) {
  outflow_val <- outflow_val + (safe_as_numeric(df[[outflow_crop_col]]) > 0)
}
if (outflow_grass_col %in% names(df)) {
  outflow_val <- outflow_val + (safe_as_numeric(df[[outflow_grass_col]]) > 0)
}
df$land_outflow <- as.numeric(outflow_val > 0)

# 参与合作社
coop_col <- "23._是否参与合作社："
if (coop_col %in% names(df)) {
  df$coop_member <- ifelse(grepl("是", df[[coop_col]]), 1, 0)
} else {
  df$coop_member <- NA
}

# 技术培训
train_col <- "27.3_是否有接受农业技术培训经历？"
if (train_col %in% names(df)) {
  df$tech_training <- ifelse(grepl("是", df[[train_col]]), 1, 0)
} else {
  df$tech_training <- NA
}

# 灌溉设施
irrigation_col <- "28.1_是否具备灌溉设施？"
if (irrigation_col %in% names(df)) {
  df$irrigation <- ifelse(grepl("是", df[[irrigation_col]]), 1, 0)
} else {
  df$irrigation <- NA
}

# 农业保险参与（根据问卷，可能没有直接问，暂时留空）
df$ag_insurance <- NA

# ==================== 6.8 适应意愿变量 ====================
cat("  - 适应意愿变量\n")

future_intent_col <- "50._未来3–5年家庭发展意向_可多选_:"
if (future_intent_col %in% names(df)) {
  intent_raw <- df[[future_intent_col]]
  intent_raw[is.na(intent_raw)] <- ""
  
  # 初始化意愿变量
  df$intent_expand_crop <- 0
  df$intent_expand_livestock <- 0
  df$intent_offfarm <- 0
  df$intent_new_biz <- 0
  
  # 根据关键词匹配（需根据实际选项调整）
  for (i in 1:nrow(df)) {
    txt <- intent_raw[i]
    if (nchar(txt) > 0) {
      if (grepl("扩大种植|扩大种", txt)) df$intent_expand_crop[i] <- 1
      if (grepl("扩大养殖|扩大畜牧|扩大种", txt)) df$intent_expand_livestock[i] <- 1
      if (grepl("外出务工|务工为主", txt)) df$intent_offfarm[i] <- 1
      if (grepl("探索副业|非农经济|加入合作社|新型经营主体|生态农业", txt)) df$intent_new_biz[i] <- 1
    }
  }
} else {
  df$intent_expand_crop <- df$intent_expand_livestock <- df$intent_offfarm <- df$intent_new_biz <- NA
}

# ==================== 6.9 认知感知变量 ====================
cat("  - 认知感知变量\n")

# 风险感知（干旱、市场等）—— 问卷中可能没有直接评分，暂时留空
df$risk_drought <- NA
df$risk_market <- NA

# 结果期望（种养调整收入预期）—— 问卷中可能没有直接评分，暂时留空
df$expect_income <- NA

# 他人行为感知 —— 问卷中可能没有直接询问，暂时留空
df$peer_behavior <- NA

# ==================== 6.10 政策变量 ====================
cat("  - 政策变量\n")

# 是否享受过相关政策补贴
policy_support_col <- "35._您是否享受过以下农业或养殖相关政策补贴_可多选"
if (policy_support_col %in% names(df)) {
  df$policy_support <- ifelse(!is.na(df[[policy_support_col]]) & df[[policy_support_col]] != "", 1, 0)
} else {
  df$policy_support <- NA
}

# 生态政策参与（以退耕还林为例）
eco_policy_col <- "1.退耕还林_还草_是否参与"
if (eco_policy_col %in% names(df)) {
  df$eco_policy <- ifelse(grepl("是", df[[eco_policy_col]]), 1, 0)
} else {
  df$eco_policy <- NA
}

# ------------------------------ 7. 数据质量检查 ------------------------------
cat("\n步骤5：数据质量检查\n")

# 缺失值统计
missing_stats <- data.frame(
  变量 = c("gender", "age", "edu_level", "labor_num", "cultivated_area",
         "grassland_area", "livestock_su", "income_crop_share",
         "income_livestock_share", "changed_industry"),
  缺失数 = sapply(c("gender", "age", "edu_level", "labor_num", "cultivated_area",
                 "grassland_area", "livestock_su", "income_crop_share",
                 "income_livestock_share", "changed_industry"),
               function(v) sum(is.na(df[[v]]))),
  缺失比例 = sapply(c("gender", "age", "edu_level", "labor_num", "cultivated_area",
                  "grassland_area", "livestock_su", "income_crop_share",
                  "income_livestock_share", "changed_industry"),
                function(v) round(mean(is.na(df[[v]])) * 100, 2))
)
print(missing_stats)

# 异常值检查
cat("\n异常值检查：\n")
if (any(!is.na(df$income_crop_share))) {
  cat("种植收入占比范围：", range(df$income_crop_share, na.rm = TRUE), "\n")
} else {
  cat("种植收入占比全为缺失\n")
}
if (any(!is.na(df$income_livestock_share))) {
  cat("畜牧收入占比范围：", range(df$income_livestock_share, na.rm = TRUE), "\n")
} else {
  cat("畜牧收入占比全为缺失\n")
}
cat("耕地面积范围：", range(df$cultivated_area, na.rm = TRUE), "\n")
cat("畜地比范围：", range(df$stocking_density, na.rm = TRUE), "\n")

# ------------------------------ 8. 保存清洗结果 ------------------------------
cat("\n步骤6：保存清洗结果\n")

# 8.1 保存完整数据（含所有原始列和计算列）
write_xlsx(df, file.path(output_dir, "清洗后完整数据.xlsx"))

# 8.2 保存核心变量（论文分析所需）
core_vars <- c(
  # 标识
  "提交答卷时间", "调研员", "所属区县", "village",
  # 基础人口
  "gender", "age", "family_size", "labor_num", "edu_level",
  # 区域
  "county", "county_dingbian", "county_yuyang", "county_suide",
  # 收入结构
  "income_total", "income_crop_share", "income_livestock_share",
  "income_offfarm_share", "income_subsidy_share",
  # 资源禀赋
  "cultivated_area", "grassland_area", "total_land", "abandoned_area",
  "cattle_num", "sheep_num", "pig_num", "chicken_num", "livestock_su", "stocking_density",
  # 作物
  "crop_diversity", "corn_area", "fodder_ratio", "fodder_self_sufficiency",
  # 适应行为
  "changed_industry", "land_inflow", "land_outflow", "coop_member",
  "tech_training", "irrigation", "ag_insurance",
  # 适应意愿
  "intent_expand_crop", "intent_expand_livestock", "intent_offfarm", "intent_new_biz",
  # 认知感知
  "risk_drought", "risk_market", "expect_income", "peer_behavior",
  # 政策
  "policy_support", "eco_policy"
)

# 仅保留存在的变量
core_vars_exist <- intersect(core_vars, names(df))
df_core <- df[core_vars_exist]

write_xlsx(df_core, file.path(output_dir, "清洗后核心变量.xlsx"))

# 8.3 生成变量映射表
cat("  - 生成变量映射表\n")
var_mapping <- data.frame(
  变量英文名 = names(df_core),
  变量中文名 = NA,
  来源说明 = NA,
  数据类型 = sapply(df_core, function(x) class(x)[1]),
  样本数 = sapply(df_core, function(x) sum(!is.na(x))),
  缺失数 = sapply(df_core, function(x) sum(is.na(x))),
  均值或分布 = sapply(df_core, function(x) {
    if (is.numeric(x)) {
      sprintf("均值=%.2f", mean(x, na.rm = TRUE))
    } else {
      paste(names(table(x))[1:min(3, length(table(x)))], collapse = ",")
    }
  })
)

# 手动补充中文名（可后续完善）
write_xlsx(var_mapping, file.path(output_dir, "变量映射表.xlsx"))

# 8.4 生成清洗报告
cat("  - 生成清洗报告\n")
report_lines <- c(
  paste(rep("=", 60), collapse = ""),
  "陕北农牧交错区农户调查数据清洗报告",
  paste(rep("=", 60), collapse = ""),
  paste("生成时间:", Sys.time()),
  paste("原始数据文件:", input_path),
  paste("输出目录:", output_dir),
  "",
  "一、数据基本信息",
  paste("原始样本数:", nrow(raw_data)),
  paste("清洗后样本数:", nrow(df)),
  paste("核心变量数:", ncol(df_core)),
  "",
  "二、样本分布",
  paste("定边县:", sum(df$county_dingbian, na.rm = TRUE)),
  paste("榆阳区:", sum(df$county_yuyang, na.rm = TRUE)),
  paste("绥德县:", sum(df$county_suide, na.rm = TRUE)),
  "",
  "三、核心变量描述性统计",
  paste("种植收入占比均值:", round(mean(df$income_crop_share, na.rm = TRUE) * 100, 1), "%"),
  paste("畜牧收入占比均值:", round(mean(df$income_livestock_share, na.rm = TRUE) * 100, 1), "%"),
  paste("耕地面积均值:", round(mean(df$cultivated_area, na.rm = TRUE), 1), "亩"),
  paste("草地面积均值:", round(mean(df$grassland_area, na.rm = TRUE), 1), "亩"),
  paste("畜地比均值:", round(mean(df$stocking_density, na.rm = TRUE), 2)),
  "",
  "四、适应性经营响应",
  paste("调整主导产业比例:", round(mean(df$changed_industry, na.rm = TRUE) * 100, 1), "%"),
  paste("土地流入比例:", round(mean(df$land_inflow, na.rm = TRUE) * 100, 1), "%"),
  paste("土地流出比例:", round(mean(df$land_outflow, na.rm = TRUE) * 100, 1), "%"),
  paste("合作社参与比例:", round(mean(df$coop_member, na.rm = TRUE) * 100, 1), "%"),
  paste("技术培训比例:", round(mean(df$tech_training, na.rm = TRUE) * 100, 1), "%"),
  "",
  "五、适应意愿",
  paste("扩大种植意愿:", round(mean(df$intent_expand_crop, na.rm = TRUE) * 100, 1), "%"),
  paste("扩大养殖意愿:", round(mean(df$intent_expand_livestock, na.rm = TRUE) * 100, 1), "%"),
  paste("增加务工意愿:", round(mean(df$intent_offfarm, na.rm = TRUE) * 100, 1), "%"),
  paste("发展新产业意愿:", round(mean(df$intent_new_biz, na.rm = TRUE) * 100, 1), "%")
)
writeLines(report_lines, file.path(output_dir, "数据清洗报告.txt"))

# ------------------------------ 9. 完成 ---------------------------------------
end_time <- Sys.time()
run_time <- difftime(end_time, start_time, units = "mins")

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("数据清洗完成！\n")
cat("运行时间:", round(run_time, 2), "分钟\n")
cat("输出文件保存在:", output_dir, "\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
