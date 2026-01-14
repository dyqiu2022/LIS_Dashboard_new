
#' 对数变换函数 (Log1p Transformation)
#'
#' @description 对输入向量执行 log(x + 1) 变换，支持自定义底数。
#' 相比直接使用 log(x + 1)，该函数在处理极小数值时具有更好的数值稳定性。
#'
#' @param input 数值向量，待变换的数据。
#' @param base 数值，对数的底数（默认为 e）。
#'
#' @return 返回变换后的数值向量。
#' @export
#'
#' @examples
#' log_trans(c(0, 1, 10), base = 10)
log_trans <- function(input, base = exp(1)) {
  base::log1p(input) / base::log(base)
}

#' 计算值在数据集中的分位数位置 (Mid-rank Percentile)
#'
#' @description
#' 使用标准分位数定义（Mid-rank 方法）计算给定数值在数据集中的位置。
#' 该位置定义为：(小于等于该值的样本数 + 小于该值的样本数) / (2 * 总样本数)。
#'
#' @param data 数值向量，参考数据集。
#' @param value 数值或数值向量，需要计算位置的目标值。
#'
#' @return 返回一个 0 到 1 之间的数值向量，代表位置。
#'
#' @importFrom stats ecdf
#' @export
#'
#' @examples
#' get_quantile_position(rnorm(100), 0)
get_quantile_position <- function(data, value) {
  n <- length(data)
  if (n == 0) return(NA)

  # ecdf(x)(v) 返回的是 P(X <= v)
  f_le <- stats::ecdf(data)

  # 为了实现 Mid-rank (le + lt)/2n
  # 我们需要 P(X <= v) 和 P(X < v)
  # 在离散数据中，P(X < v) 可以通过一个小偏移量或者计算 P(X <= v) 减去刚好等于 v 的概率得到

  vapply(value, function(v) {
    le_prop <- f_le(v)               # 小于等于 v 的比例 (le/n)
    # 小于 v 的比例可以通过计算比 v 稍微小一点点的点来实现
    lt_prop <- sum(data < v, na.rm = TRUE) / n

    (le_prop + lt_prop) / 2
  }, FUN.VALUE = numeric(1))
}


#' 获取公式中变量的基准水平 (Baseline Levels)
#'
#' @description
#' 解析传入的公式字符串，从数据集中提取出所有相关的原始变量，
#' 并为每个变量计算一个“代表性”的基准值：数值型变量取中位数 (Median)，
#' 分类变量取众数 (Mode)。
#'
#' @param formula_str 字符型，合法的 R 公式字符串（例如 "y ~ x1 + x2:x3"）。
#' @param data 数据框，包含公式中所引用的变量。
#'
#' @return 返回一个只有一行的 data.frame，包含公式中所有自变量的基准值。
#'
#' @importFrom stats as.formula terms median
#' @export
#'
#' @examples
#' # 使用内置数据集 iris
#' get_baseline_level("Sepal.Length ~ Sepal.Width + Species", iris)
get_baseline_level <- function(formula_str, data) {
  # 1. 解析公式获取所有变量
  # 吐槽提醒：其实 R 有一个内置函数 all.vars() 可以直接提取公式里所有的变量名
  formula_obj <- stats::as.formula(formula_str)

  # 获取自变量（去掉公式左边的响应变量）
  # 使用 all.vars 比你自己写复杂的 gsub 正则表达式要稳健得多
  all_vars <- base::all.vars(formula_obj)
  # 如果公式有左手边(y)，all.vars 的第一个通常是 y，需要根据情况处理
  # 这里保留你原有的 terms 逻辑来确保只提取预测变量
  raw_vars <- base::attr(stats::terms(formula_obj), "term.labels")

  # 进一步提取原始变量名（处理交互项 x1:x2 和函数包装 I(x^2)）
  # 严谨写法建议：利用 stats::model.frame 自动提取
  clean_vars <- base::unique(base::unlist(base::strsplit(raw_vars, ":")))
  # 去除 I() 等函数包装
  clean_vars <- base::gsub(".*\\(([^)]+)\\).*", "\\1", clean_vars)
  clean_vars <- base::unique(clean_vars)

  # 初始化结果数据框
  baseline_df <- base::data.frame(base::matrix(ncol = base::length(clean_vars), nrow = 1))
  base::colnames(baseline_df) <- clean_vars

  # 处理每个原始变量
  for (var in clean_vars) {
    if (var %in% base::names(data)) {
      col_data <- data[[var]]

      # 数值变量处理
      if (base::is.numeric(col_data) || base::is.integer(col_data)) {
        baseline_df[[var]] <- stats::median(col_data, na.rm = TRUE)
      }
      # 离散变量处理
      else if (base::is.factor(col_data) || base::is.character(col_data) || base::is.logical(col_data)) {
        freq_table <- base::table(col_data, useNA = "no")
        baseline_df[[var]] <- base::names(freq_table)[base::which.max(freq_table)]
      }
      else {
        base::warning(base::paste("变量", var, "类型不支持，使用第一个值"))
        baseline_df[[var]] <- col_data[1]
      }
    } else {
      base::warning(base::paste("变量", var, "在数据中不存在，设为NA"))
      baseline_df[[var]] <- NA
    }
  }

  return(baseline_df)
}


#' 将 HSL 颜色空间转换为 RGB 颜色空间
#'
#' @description
#' 将色相 (Hue)、饱和度 (Saturation) 和亮度 (Lightness) 转换为 RGB 数值。
#' 该函数完全基于 Base R 实现，无外部依赖，且支持向量化输入。
#'
#' @param h 数值或数值向量。色相，取值范围 [0, 1]。
#' @param s 数值或数值向量。饱和度，取值范围 [0, 1]。
#' @param l 数值或数值向量。亮度，取值范围 [0, 1]。
#'
#' @return 返回一个矩阵，包含三列：Red, Green, Blue。数值范围为 [0, 255]。
#'
#' @details
#' 如果输入是向量，所有参数的长度应一致，或遵循 R 的循环补齐规则。
#'
#' @export
#'
#' @examples
#' # 单个颜色
#' hsl_to_rgb(0.5, 0.5, 0.5)
#'
#' # 向量化处理：同时转换多个颜色
#' hsl_to_rgb(c(0, 0.33, 0.66), 0.5, 0.5)
hsl_to_rgb <- function(h, s, l) {
  # 1. 确保输入在合理范围内
  h <- h %% 1

  # 2. 定义核心转换逻辑（向量化辅助函数）
  # 使用 base R 替代 dplyr::case_when 以提升性能并减少依赖
  hue_to_rgb <- function(p, q, t) {
    t <- (t %% 1) # 确保 t 在 [0, 1] 之间 (处理 t < 0 或 t > 1 的情况)

    # 预分配结果向量
    res <- p

    # 向量化条件判断
    # 这里的逻辑等同于 case_when，但速度更快且无需依赖
    mask1 <- t < 1/6
    mask2 <- t >= 1/6 & t < 1/2
    mask3 <- t >= 1/2 & t < 2/3

    # 公式计算
    res[mask1] <- p[mask1] + (q[mask1] - p[mask1]) * 6 * t[mask1]
    res[mask2] <- q[mask2]
    res[mask3] <- p[mask3] + (q[mask3] - p[mask3]) * (2/3 - t[mask3]) * 6

    return(res)
  }

  # 3. 计算 q 和 p
  q <- ifelse(l < 0.5, l * (1 + s), l + s - l * s)
  p <- 2 * l - q

  # 4. 计算 RGB 分量
  r <- hue_to_rgb(p, q, h + 1/3)
  g <- hue_to_rgb(p, q, h)
  b <- hue_to_rgb(p, q, h - 1/3)

  # 5. 处理饱和度为 0 的情况 (灰度)
  # 如果 s 为 0，RGB 都等于 l
  is_gray <- (s == 0)
  if (any(is_gray)) {
    # 考虑输入可能是标量或向量，这里做广播处理可能比较复杂
    # 但由于上面的公式在 s=0 时，q=l, p=l，带入 hue_to_rgb 也能算出正确结果 (都是 l)
    # 所以其实不需要单独写 if(s==0)，上面的通用公式已经覆盖了 s=0 的情况！
    # 数学证明：s=0 -> q=l -> p=2l-l=l -> hue_to_rgb 返回 p 即 l。
    # 所以代码可以保持简洁，不需要额外的 if 分支。
  }

  # 6. 格式化输出
  # 返回矩阵比返回扁平向量更专业
  rgb_mat <- cbind(R = r, G = g, B = b)

  # 限制范围并取整
  rgb_mat <- round(pmax(0, pmin(1, rgb_mat)) * 255)

  return(rgb_mat)
}

#' 根据字符串生成确定性颜色 (Deterministic Color Generation)
#'
#' @description
#' 基于字符串的 MD5 哈希值生成唯一的 HEX 颜色代码。
#' 相同的字符串永远会生成相同的颜色，适用于为用户、科室或类别分配固定颜色。
#'
#' @param text 字符型。需要转换的文本字符串。
#' @param predefined 命名字符向量（可选）。预定义的颜色映射表。
#'   如果 `text` 存在于该向量的名称中，则直接返回对应的颜色值。
#' @param saturation 数值。颜色的饱和度 [0, 1]，默认为 0.6。
#' @param lightness 数值。颜色的亮度 [0, 1]，默认为 0.6。
#'
#' @return 返回一个格式为 "#RRGGBB" 的颜色字符串。
#'
#' @importFrom digest digest
#' @importFrom stats setNames
#' @export
string_to_color <- function(text,
                            predefined = NULL,
                            saturation = 0.6,
                            lightness = 0.6) {
  # 1. 鲁棒性检查：处理空值
  if (length(text) == 0 || is.na(text) || text == "") {
    return("#CCCCCC") # 返回默认灰色
  }

  # 2. 检查预定义颜色
  if (!is.null(predefined) && text %in% names(predefined)) {
    return(predefined[[text]])
  }

  # 3. 生成 MD5 哈希
  hash_hex <- digest::digest(text, algo = "md5", serialize = FALSE)

  # 4. 提取特征值转为色相 (Hue)
  hex_subset <- substr(hash_hex, 1, 6)
  hue_val <- strtoi(hex_subset, base = 16L)
  hue <- (hue_val %% 360) / 360

  # 5. 调用内部工具函数转换为 RGB
  # 这里返回的是一个长度为3的向量 c(r, g, b)
  rgb_vec <- hsl_to_rgb(hue, saturation, lightness)

  # 6. 格式化为 HEX 颜色码
  # 修正点：去掉了逗号，直接使用 [1], [2], [3]
  sprintf("#%02X%02X%02X",
          as.integer(rgb_vec[1]),
          as.integer(rgb_vec[2]),
          as.integer(rgb_vec[3]))
}



#' 计算基于 Tukey 方法的参考区间 (Reference Interval)
#'
#' @description
#' 使用改进的 Tukey (Boxplot) 方法计算参考区间。
#' 该算法在标准 Tukey Fences (1.5 * IQR) 的基础上增加了生物医学数据的边界约束：
#' 1. 下限不会低于 0（除非数据本身包含负数）。
#' 2. 当理论下限明显不合理（如远低于数据范围）时，回退到数据最小值。
#' 3. 对于中间分位数（0.25 - 0.75），使用实际数据的分位数；对于两端，使用 Tukey Fences 截断。
#'
#' @param data_vector 数值向量。包含待分析的检测结果。
#' @param ri_perc 数值向量。需要计算的百分位数（如 c(0.025, 0.975)）。
#' @param fence_k 数值。Tukey 围栏的系数，默认为 1.5 (标准)。3.0 可用于极端异常值。
#'
#' @return 返回一个 data.frame，包含以下列：
#' \itemize{
#'   \item \code{RIperc}: 请求的百分位。
#'   \item \code{pointEst}: 点估计值（根据逻辑可能是 Fence 或 实际分位数）。
#'   \item \code{lower}: 目前与 pointEst 相同（预留字段）。
#'   \item \code{upper}: 目前与 pointEst 相同（预留字段）。
#' }
#'
#' @importFrom stats quantile IQR na.omit
#' @export
#'
#' @examples
#' # 正常数据
#' calculate_tukey_ri(rnorm(100, mean = 50, sd = 10), c(0.025, 0.975))
#'
#' # 包含异常值的数据
#' test_data <- c(rnorm(100, 50, 10), 1000)
#' calculate_tukey_ri(test_data, c(0.025, 0.5, 0.975))
calculate_tukey_ri <- function(data_vector,
                               ri_perc = c(0.025, 0.975),
                               fence_k = 1.5) {

  # 1. 数据清洗
  # 使用 as.numeric 确保输入不是字符型数字
  data <- as.numeric(stats::na.omit(data_vector))

  # 初始化结果结构 (使用 data.frame 替代 matrix)
  res_df <- data.frame(
    RIperc = ri_perc,
    pointEst = as.numeric(NA),
    lower = as.numeric(NA),
    upper = as.numeric(NA)
  )

  # 2. 样本量检查
  if (length(data) < 2) {
    # 样本不足直接返回空结果，避免计算报错
    return(res_df)
  }

  # 3. 计算基础统计量
  # type = 7 是 R 的默认分位数算法，在医学统计中常用
  q1 <- stats::quantile(data, 0.25, type = 7)
  q3 <- stats::quantile(data, 0.75, type = 7)

  # 直接使用 stats::IQR 函数更简洁
  # iqr_val <- q3 - q1
  iqr_val <- stats::IQR(data, type = 7)

  # 4. 计算 Tukey Fences
  theoretical_lower <- q1 - fence_k * iqr_val
  theoretical_upper <- q3 + fence_k * iqr_val

  data_min <- min(data)
  data_max <- max(data)

  # 5. 应用 LIS 业务规则约束
  # 下限约束：不小于0 (假设检验项目非负)，且考虑数据最小值
  # 注意：如果数据本身有负数（如碱剩余 BE），这里的 max(..., 0) 可能需要根据项目调整
  lower_fence <- max(theoretical_lower, data_min, 0)

  # 上限约束：不超过最大值
  upper_fence <- min(theoretical_upper, data_max)

  # 规则修正：如果理论下限极其不合理（< min * 0.5），强制使用最小值
  # 注意：这里保留了你的 warning，但在批量计算时建议移除，改为返回 flag
  if (theoretical_lower < data_min * 0.5) {
    warning("Tukey 下限修正：理论值远低于实际范围，已使用数据最小值代替。")
    lower_fence <- data_min
  }

  # 6. 向量化赋值 (替代 for 循环)
  # 逻辑：
  # <= 0.25 的使用下限 Fence
  # >= 0.75 的使用上限 Fence
  # 中间的 使用实际分位数

  # 标记索引
  idx_low  <- ri_perc <= 0.25
  idx_high <- ri_perc >= 0.75
  idx_mid  <- !idx_low & !idx_high

  # 批量赋值
  if (any(idx_low)) {
    res_df$pointEst[idx_low] <- lower_fence
  }

  if (any(idx_high)) {
    res_df$pointEst[idx_high] <- upper_fence
  }

  if (any(idx_mid)) {
    # 一次性计算所有中间分位数
    res_df$pointEst[idx_mid] <- stats::quantile(data, ri_perc[idx_mid], type = 7)
  }

  # 填充 CI 上下限 (目前逻辑是点估计=上下限)
  res_df$lower <- res_df$pointEst
  res_df$upper <- res_df$pointEst

  return(res_df)
}

#' 将年龄离散化为区间 (Age Discretization)
#'
#' @description
#' 将连续的年龄数值离散化为有序的分类变量（Factor）。
#' 自动处理分箱断点，生成的标签格式为 "Start-End"（例如 "0-2", "3-5"）。
#' 注意：标签生成逻辑假设年龄为整数精度（左闭右开区间 [a, b) 显示为 a-(b-1)）。
#'
#' @param age_vector 数值向量。包含待分组的年龄。
#' @param grain 正整数。分箱的颗粒度（步长），默认为 3 岁。
#'
#' @return 返回一个有序因子 (Ordered Factor)，包含离散化后的年龄段。
#' 如果输入全是 NA，则返回全 NA 的因子。
#'
#' @export
#'
#' @examples
#' # 正常情况
#' ages <- c(1, 4, 10, 15, 80)
#' discretize_age(ages, grain = 10)
#'
#' # 处理 NA 和脏数据
#' ages_dirty <- c(1, NA, 5, -1) # 负数会报错，NA 会保留
#' try(discretize_age(ages_dirty))
discretize_age <- function(age_vector, grain = 3) {
  # 1. 基础类型检查
  if (!is.numeric(age_vector)) {
    stop("输入 'age_vector' 必须是数值向量。")
  }

  if (!is.numeric(grain) || grain <= 0) {
    stop("颗粒度 'grain' 必须为正数。")
  }

  # 2. 脏数据防御 (NA 处理)
  # 必须加 na.rm = TRUE，否则遇到 NA 程序直接崩
  if (any(age_vector < 0, na.rm = TRUE)) {
    stop("年龄数据中包含负数值，请检查数据质量。")
  }

  # 如果全是 NA，直接返回相同长度的 NA 向量（保持结构一致）
  # 这一步是为了防止 min() 报 warning
  valid_ages <- age_vector[!is.na(age_vector)]
  if (length(valid_ages) == 0) {
    return(factor(rep(NA, length(age_vector)), ordered = TRUE))
  }

  # 3. 计算分箱断点
  # 使用 valid_ages 避免 min/max 因为 NA 报错
  min_val <- min(valid_ages)
  max_val <- max(valid_ages)

  # 这里的逻辑很棒：max + grain 确保了最大值一定能被包进去
  # floor 确保断点是整数，看起来更整洁
  start_point <- floor(min_val / grain) * grain
  breaks <- seq(from = start_point, to = max_val + grain, by = grain)

  # 4. 向量化生成标签 (替代 sapply 循环)
  # 逻辑：取 breaks 的前 n-1 个作为起点，后 n-1 个减 1 作为终点
  # 例如 breaks: 0, 3, 6 -> lower: 0, 3 -> upper: 2, 5 -> labels: "0-2", "3-5"
  lower_bounds <- breaks[-length(breaks)]
  upper_bounds <- breaks[-1] - 1
  bin_labels <- paste0(lower_bounds, "-", upper_bounds)

  # 5. 执行分箱
  cut(
    age_vector,
    breaks = breaks,
    right = FALSE,        # 左闭右开 [0, 3) -> 包含 0, 1, 2
    labels = bin_labels,
    ordered_result = TRUE # 生成有序因子，方便后续画图排序
  )
}

#' 多条件临床诊断文本匹配与聚类
#'
#' @description
#' 判断输入的临床文本（可能包含多个由标点分隔的诊断）是否满足特定的筛选逻辑。
#' 逻辑如下：
#' 1. 将文本分割为多个片段。
#' 2. 对于任意一个片段，若同时满足以下条件，则判定为 TRUE：
#'    - 满足 `list_condition` 中定义的所有“且”条件组（组内为“或”逻辑）。
#'    - 不包含 `not` 中的关键词，除非该片段包含 `not_restric` 中的豁免词。
#'
#' @param info_combined 字符型。包含一个或多个诊断的原始字符串。
#' @param list_condition 列表 (List)。每个元素是一个字符向量，代表一组“或”逻辑的关键词。
#'   列表的不同元素之间是“且”逻辑。
#'   特殊值 "ALL" 表示该组条件无条件通过。
#' @param not 字符向量。排除关键词列表（黑名单）。
#' @param not_restric 字符向量。排除逻辑的豁免关键词列表（白名单）。
#'
#' @return 逻辑值 (TRUE/FALSE)。只要有一个片段满足条件即返回 TRUE。
#'
#' @export
#'
#' @examples
#' # 定义条件：必须包含 ("糖尿病" 或 "DM") AND ("2型" 或 "II型")
#' conds <- list(c("糖尿病", "DM"), c("2型", "II型"))
#' # 定义排除：不能有 "妊娠"
#' black <- c("妊娠")
#' # 定义豁免：如果有 "既往"，则忽略排除逻辑（例如 "既往妊娠糖尿病" 可能要算？）
#' white <- c("既往")
#'
#' # 测试
#' text1 <- "高血压；2型糖尿病" # TRUE
#' text2 <- "妊娠期糖尿病"       # FALSE (中了黑名单)
#' text3 <- "既往妊娠期糖尿病"   # TRUE (中了白名单，黑名单失效)
#'
#' grouping_muti_groups(text1, conds, black, white)
grouping_muti_groups <- function(info_combined, list_condition, not, not_restric) {
  # 1. 预处理：将长文本切割成片段
  # 注意：保留了原有的括号作为分隔符的逻辑
  raw_segments <- base::unlist(base::strsplit(base::as.character(info_combined), split = "[；，,;|()（）]"))

  # 去除切割后可能产生的空字符串
  segments <- raw_segments[raw_segments != ""]

  if (length(segments) == 0) return(FALSE)

  # 2. 核心逻辑：只要有一个片段符合要求，结果就是 TRUE (base::any)
  # 使用 vapply 替代 for 循环，代码更紧凑且速度更快
  base::any(base::vapply(segments, function(seg) {
    check_single_segment(seg, list_condition, not, not_restric)
  }, FUN.VALUE = logical(1)))
}

#' 检查单个文本片段是否符合逻辑 (内部辅助函数)
#'
#' @param seg 单个文本片段字符串
#' @param conds 条件列表
#' @param black_list 排除列表
#' @param white_list 豁免列表
#'
#' @return 逻辑值
#' @noRd
check_single_segment <- function(seg, conds, black_list, white_list) {

  # --- Step 1: 检查 AND 组合逻辑 ---
  # 必须满足 conds 里的每一组 (all)
  pass_and <- base::all(base::vapply(conds, function(group) {
    # 如果组里有 "ALL"，直接通过
    if ("ALL" %in% group) return(TRUE)

    # 组内是 OR 逻辑：只要命中一个词即可 (any)
    # 使用 fixed = TRUE 进行精确子串匹配
    base::any(base::vapply(group, function(k) {
      base::grepl(k, seg, fixed = TRUE)
    }, FUN.VALUE = logical(1)))

  }, FUN.VALUE = logical(1)))

  # 如果连肯定条件都没过，直接返回 FALSE，不再跑后面的逻辑
  if (!pass_and) return(FALSE)

  # --- Step 2: 检查 NOT 排除逻辑 ---

  # 2.1 检查是否豁免 (白名单)
  # 如果命中了豁免词，则无需进行黑名单检查，直接通过
  is_protected <- base::any(base::vapply(white_list, function(w) {
    base::grepl(w, seg, fixed = TRUE)
  }, FUN.VALUE = logical(1)))

  if (is_protected) return(TRUE)

  # 2.2 检查是否排除 (黑名单)
  has_bad_word <- base::any(base::vapply(black_list, function(b) {
    base::grepl(b, seg, fixed = TRUE)
  }, FUN.VALUE = logical(1)))

  # 如果有坏词，返回 FALSE；否则返回 TRUE
  return(!has_bad_word)
}


#' 时间变量离散化 (Time Discretization)
#'
#' @description
#' 将时间对象（POSIXct 或 Date）按指定颗粒度转换为有序因子（Ordered Factor）。
#'
#' @details
#' 针对 "week" 颗粒度，函数采用 **ISO 8601 标准周**定义：
#' \itemize{
#'   \item 使用 `%G` (ISO年份) 而非 `%Y` (日历年份) 来避免跨年周的排序错误。
#'   \item 格式生成为 "YYYY-Wxx"。
#' }
#' 这种处理确保了如 2021-01-01 (属于2020年第53周) 会被正确标记为 "2020-W53"，
#' 从而在绘图时保持正确的时间顺序。
#'
#' @param datetime POSIXct 或 Date 类型的向量。需要离散化的时间数据。
#' @param grain 字符型。时间颗粒度，可选值为 "month", "week", "day"。默认为 "month"。
#'
#' @return 返回一个有序因子 (Ordered Factor)。因子的 Levels 按照时间先后顺序排列。
#'
#' @export
#'
#' @examples
#' # 1. 基础用法
#' dates <- as.POSIXct(c("2023-01-15", "2023-02-20", "2023-01-10"))
#' discretize_time(dates, "month")
#'
#' # 2. 跨年周测试 (修复了原函数的 Bug)
#' # 2021-01-01 其实是 2020年的第53周
#' tricky_date <- as.POSIXct("2021-01-01")
#'
#' # 错误做法结果：2021-W53 (看起来像2021年底)
#' # 正确做法结果：2020-W53 (正确归类到时间轴的开头)
#' discretize_time(tricky_date, "week")
discretize_time <- function(datetime, grain = c("month", "week", "day")) {
  # 1. 鲁棒性检查：支持 POSIXt 和 Date
  if (!inherits(datetime, "POSIXt") && !inherits(datetime, "Date")) {
    stop("输入 'datetime' 必须是 POSIXct/POSIXlt 或 Date 格式。")
  }

  # 匹配参数，处理缩写 (如输入 "m" 自动匹配 "month")
  grain <- match.arg(grain)

  # 2. 核心离散化逻辑
  time_label <- switch(grain,
                       "month" = format(datetime, "%Y-%m"),
                       "week"  = {
                         # 关键修正：使用 %G (ISO Year) 配合 %V (ISO Week)
                         # %G 保证了当日期是1月但属于上一周时，年份显示为上一年
                         format(datetime, "%G-W%V")
                       },
                       "day"   = format(datetime, "%Y-%m-%d")
  )

  # 3. 生成有序因子
  # 利用 unique(x[order(datetime)]) 确保 Factor 的 Levels 是按时间顺序排列的
  # 而不仅仅是按字符串字母顺序 (虽然对于 ISO 格式两者通常一致)
  if (length(datetime) == 0) {
    return(factor(levels = character(0), ordered = TRUE))
  }

  factor(
    time_label,
    levels = unique(time_label[order(datetime)]),
    ordered = TRUE
  )
}


#' 安全地构建公式对象 (Safe Formula Construction)
#'
#' @description
#' 将响应变量名和预测变量字符串转换为 R 的公式对象。
#' 包含完整的错误捕获机制，防止因字符串格式错误导致程序崩溃。
#'
#' @param formula_str 字符型。公式的右侧部分 (RHS)，例如 "x1 + x2" 或 "poly(x, 2)"。
#' @param response_var 字符型。响应变量名 (LHS)。自动处理变量名中的空格或特殊字符。
#'
#' @return 成功时返回 \code{formula} 对象，失败时返回 \code{NULL}。
#'
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' # 1. 正常转换
#' safe_formula("Age + Sex", "Outcome")
#'
#' # 2. 处理带空格的变量名 (自动加反引号)
#' safe_formula("Age + Sex", "Time to Event")
#'
#' # 3. 错误的输入 (优雅返回 NULL 而不崩溃)
#' safe_formula(NULL, "y")
#' safe_formula("x", NULL)
safe_formula <- function(formula_str, response_var) {
  # 1. 基础输入验证
  # 必须确保两个输入都是非空的单一字符串
  if (!is.character(formula_str) || length(formula_str) != 1 ||
      !is.character(response_var) || length(response_var) != 1) {
    return(NULL)
  }

  tryCatch({
    # 2. 处理特殊变量名 (关键改进)
    # 如果 response_var 是 "Time (Days)"，直接 paste 会报错
    # 我们检查它是否已经被反引号包围，如果没有，且包含特殊字符，建议加上
    # 这里为了简便，简单粗暴地检查是否需要加反引号有点复杂
    # 更稳妥的方式是假设用户传进来的是原始列名，我们在拼接时加上反引号

    # 只有当 response_var 不包含 ` 时才加，避免重复加
    lhs <- if (grepl("^`.*`$", response_var)) response_var else paste0("`", response_var, "`")

    # 3. 构建公式字符串
    full_str <- paste(lhs, "~", formula_str)

    # 4. 转换为公式 (无需 @importFrom base paste)
    # 默认环境设置为 parent.frame()，这在 Shiny 中通常是没问题的
    stats::as.formula(full_str)

  }, error = function(e) {
    # 生产环境中，底层函数最好保持安静，除非是在 debug 模式
    # 这里我们选择静默失败，由上层逻辑判断 NULL
    return(NULL)
  })
}

#' 计算模型在指定点的平滑导数 (数值微分)
#'
#' @description
#' 使用中心差分法 (Central Difference) 计算拟合模型的导数。
#' 针对数据分布采用了自适应步长策略，并利用向量化操作优化了预测性能。
#'
#' @param fit 模型对象。支持 predict 方法返回矩阵或数据框的模型 (如 rms::cph, survival::coxph)。
#' @param x_points 数值向量。需要计算导数的 x 坐标点。
#' @param var_name 字符型。模型中对应的变量名（用于构建 newdata）。
#' @param h_factor 数值。基础步长系数，基于 IQR 计算。
#' @param shrink_factor 数值。在左侧边缘区域的步长缩放因子。
#'
#' @return 返回一个数值向量，表示对应 x 点的导数（斜率）。
#'
#' @importFrom stats IQR predict
#' @export
#'
#' @examples
#' # 假设 fit 是一个 cph 模型，计算 Age 变量在不同点的效应变化率
#' # smooth_derivative(fit, data$Age, "Age")
smooth_derivative <- function(fit, x_points, var_name,
                              h_factor = 0.001, shrink_factor = 0.1) {

  # 1. 基础数据准备
  # 移除 NA 以避免计算错误，但需注意这会导致输出长度与输入不一致
  # 建议由外部处理 NA，这里仅做检查
  if (any(is.na(x_points))) stop("x_points 中包含 NA，请先清洗数据。")

  # 计算全局边界，避免在循环中重复计算 min/max
  min_x <- min(x_points)
  max_x <- max(x_points)

  # 2. 计算自适应步长向量 (Vectorized)
  x_iqr <- stats::IQR(x_points)
  h_base <- h_factor * x_iqr

  # 确定阈值
  # 吐槽：ceiling(0.002 * N) 可能为 1，建议加一个最小行数保护
  x_sorted <- sort(x_points)
  n_thresh <- max(1, ceiling(0.002 * length(x_sorted)))
  threshold <- x_sorted[n_thresh]

  # 向量化生成 h 值
  h_values <- ifelse(x_points < threshold, shrink_factor * h_base, h_base)

  # 3. 准备左右两侧的坐标 (Vectorized)
  # 使用 pmax/pmin 进行向量化的边界截断
  x_left_vec  <- pmax(x_points - h_values, min_x)
  x_right_vec <- pmin(x_points + h_values, max_x)

  # 4. 批量预测 (关键性能优化)
  # 将所有需要预测的点拼接到一起，只调用一次 predict
  # 注意：我们需要知道变量名才能构建合法的 newdata
  all_query_x <- c(x_left_vec, x_right_vec)

  # 动态构建 data.frame，列名为 var_name
  # setNames 也是 base 包的神器
  new_data <- stats::setNames(data.frame(all_query_x), var_name)

  # 这里假设 fit 是 rms 包的模型，predict 返回矩阵，取某一列（通常是 log hazard 或 risk）
  # 注意：不同模型 predict 返回结构不同，这里保留你原本的 [, 2] 逻辑
  # 建议加 tryCatch 或根据模型类型自适应
  preds <- predict(fit, newdata = new_data)

  # 提取预测值
  # 如果 preds 是向量（如 lm），直接使用；如果是矩阵，取第2列
  if (is.matrix(preds) || is.data.frame(preds)) {
    y_values <- preds[, 2]
  } else {
    y_values <- preds
  }

  # 拆分回左右两部分
  n <- length(x_points)
  y_left  <- y_values[1:n]
  y_right <- y_values[(n + 1):(2 * n)]

  # 5. 计算导数 (Vectorized)
  # 差分公式： (y_right - y_left) / (x_right - x_left)
  # 去掉了 abs()，保留方向信息
  numerator <- y_right - y_left
  denominator <- x_right_vec - x_left_vec

  # 防止除以零（虽然有步长，但防止 min/max 导致的重合）
  dy_dx <- ifelse(denominator == 0, 0, numerator / denominator)

  return(dy_dx)
}

#' @title 批量读取 Excel 数据 (优化版)
#' @description 使用 lapply 高效读取 Excel 文件列表。
#' @param location_lst Character vector. 文件路径列表。
#' @return List. 数据框列表。
#' @importFrom readxl read_excel
#' @export
get_data <- function(location_lst) {
  # lapply 会自动预分配内存并返回列表，比 for 循环快且代码更简洁
  lapply(location_lst, function(x) {
    # 可以在这里加一个简单的错误处理，防止某一个文件坏了导致整个程序崩溃
    tryCatch({
      readxl::read_excel(x)
    }, error = function(e) {
      warning(paste("读取文件失败:", x))
      return(NULL) # 失败时返回 NULL 而不是报错中断
    })
  })
}

#' 安全日期转换函数
#'
#' @description
#' 该函数尝试将输入转换为 POSIXct 格式。它首先尝试直接转换，
#' 如果失败（例如处理 Excel 导入的数字日期），则尝试将其作为 Excel 数值日期进行转换。
#'
#' @param x 输入的日期向量（可以是字符、数字或日期格式）。
#'
#' @return 返回一个 POSIXct 类型的日期对象。如果转换失败则返回 NA。
#'
#' @importFrom lubridate ddays
#' @export
#'
#' @examples
#' safe_date_convert("2023-01-01")
#' safe_date_convert(44927) # Excel 格式日期
safe_date_convert <- function(x) {
  tryCatch({
    # 1. 首先尝试直接作为 POSIXct 转换
    # 显式使用 base 里的函数
    result <- as.POSIXct(x, tz = "UTC")
    return(result)
  }, error = function(e) {
    # 2. 如果直接转换失败，尝试 Excel 序列日期转换
    tryCatch({
      numeric_value <- as.numeric(x)
      # Excel 日期起点是 1899-12-30
      # 使用 lubridate::ddays 确保精度
      as.POSIXct("1899-12-30", tz = "UTC") + lubridate::ddays(numeric_value)
    }, error = function(e2) {
      # 3. 如果两种方法都失败，返回 NA（保持 POSIXct 类型）
      return(as.POSIXct(NA, tz = "UTC"))
    })
  })
}


# 分词与词向量函数
get_vec_ <- function(word, embedding_matrix) {
  if (word %in% rownames(embedding_matrix)) {
    embedding_matrix[word, , drop = FALSE]
  } else {
    matrix(NA)
  }
}

# 函数，用于将分词结果与词性相连接
named_lst_paste <- function(named_lst){
  paste0(named_lst, "(", named_lst %>% names, ")")
}



# 自定义可追踪进度的kmeans
kmeans_with_progress <- function(x, centers, iter.max = 10L, nstart = 1L) {
  # 初始化进度条
  withProgress(message = 'Running k-means', value = 0, {
    best <- Inf
    for (i in seq_len(nstart)) {
      incProgress(1/nstart, detail = paste("Initialization", i, "of", nstart))

      # 调用原始kmeans（单次nstart）
      Z <- stats::kmeans(x, centers, iter.max = iter.max, nstart = 1)

      # 保留最佳结果
      if (sum(Z$withinss) < best) {
        best_result <- Z
        best <- sum(Z$withinss)
      }
    }
  })
  best_result
}

# 定义变换函数
boxcox_transform <- function(x, lambda) {
  if (lambda == 0) log(x) else (x^lambda - 1)/lambda
}

# 定义反变换函数
boxcox_rev <- function(y, lambda) {
  if (lambda == 0) exp(y) else (lambda*y + 1)^(1/lambda)
}

## 函数，用一个滑动窗口，计算数据中的定量值的置信区间，返回一个dataframe, 每个列是一个相应的置信区间的数据，有一列"CI_%"作为不同置信区间的类别区分(横坐标为年龄)
## 2024/12/03修改，已改为用百分位数估计代替正态估计
get_CI <- function(data,CI,win_width,min_num){
  CI2 <- as.numeric(gsub("%", "", CI)) / 100

  qnorm_975 <- qnorm(0.975)
  age_win <- c(-1:(win_width-2)) # 用一个长度为5的年龄数据窗口来做计算
  df_temp <- data.frame(matrix(ncol = 5,nrow = 0))
  colnames(df_temp) <- c("point_estimator","CI_upper","CI_lower","年龄","CI_%")
  return_lst <- list(df_temp, df_temp, df_temp, df_temp, df_temp,df_temp, df_temp, df_temp, df_temp, df_temp) # 用于记录结果的列表果

  if (nrow(data)!=0){ # 只有在data数据量不为0的情况下才进行下列计算。
    for (i in c(0:(max(data$年龄)-5))){
      age_win <- age_win+1 # 年龄窗口逐渐移动
      current_data <- data[which(data$年龄 %in% age_win),] # 当前数据窗口内的数据
      if (nrow(current_data) < min_num){ # 如果窗口内样本量小于min_num，就没有继续计算的必要了
        next
      }

      current_result <- unlist(current_data$boxcox_result)
      min_result <- current_result %>% min
      max_result <- current_result %>% max
      sd1 <- sd(current_result) # 当前数据窗口内的标准差
      n <- nrow(current_data)
      CI_num <- 0
      for (j in CI2){
        CI_num <- CI_num+1
        z_value <-  qnorm(j)
        point_estimator <- quantile(current_result, j)
        sd_of_CI <- (sd1^2/n+(sd1^2*z_value^2)/(2*n*(n-1)))^(1/2) # 对上下限CI标准差的估计值
        CI_upper <- min(point_estimator+sd_of_CI*qnorm_975, max_result) # 置信区间上界的95%上界,控制不能太过大
        CI_lower <- max(point_estimator-sd_of_CI*qnorm_975, min_result) # 置信区间上界的95%下界,控制不能太过小

        lst_temp <- list(point_estimator, CI_upper, CI_lower, median(age_win), CI[[CI_num]])
        return_lst[[CI_num]][nrow(return_lst[[CI_num]])+1,] <- lst_temp
      }
    }
  }
  return(do.call(rbind, return_lst))
}

# 函数，用于计算rq回归对数据分布的校正，并输出作图数据
get_spline_result_data <- function(rq_formula, compare_data, rq_quantiles, indp_elements, baseline, knot_num, i){

  withProgress(message = '正在计算rq_regression', value = 0, {
    # 计算分位数回归模型
    rq_models <- lapply(1:length(rq_quantiles), function(j){
      incProgress(1/length(rq_quantiles), detail = paste("\n 处理组别: ", i, " ", "rq_regression", j, "/", length(rq_quantiles)))

      rq(rq_formula, tau = rq_quantiles[[j]], data = compare_data) %>% suppressWarnings()
    })

    # 创建预测数据
    pred_data <- data.frame(
      tau = unlist(rq_quantiles),
      自变量水平 = indp_elements,
      baseline
    )

    # 获取预测值
    pred_data$predicted_value <- lapply(rq_models, function(model){
      predict(model, newdata = baseline) %>% unlist
    })

    # 处理数据
    processing_data <- pred_data[c("predicted_value", "tau")]
    colnames(processing_data) <- c("x", "y")
    processing_data$x <- unlist(processing_data$x)
    processing_data$y <- unlist(processing_data$y)

    # 添加锚点
    anchor_points <- data.frame(
      x = c(0.00001, 10),
      y = c(0, 1)
    )

    # 合并数据
    data_with_anchors <- rbind(processing_data, anchor_points)

    # 设置约束
    con <- rbind(
      c(0, 0.00001, 0),
      c(0, 10, 1)
    )

    # 确保数据按x排序
    sorted_data <- data_with_anchors[order(data_with_anchors$x), ]

    # 计算每隔4个点的节点位置
    n_points <- nrow(sorted_data)
    knot_indices <- seq(1, n_points, by = 7)

    # 确保包含第一个和最后一个点（重要边界）
    if (!1 %in% knot_indices) knot_indices <- c(1, knot_indices)
    if (!n_points %in% knot_indices) knot_indices <- c(knot_indices, n_points)

    # 获取实际的x值作为节点
    knots_custom <- sorted_data$x[knot_indices] %>% unique() %>% sort()

    # 确保节点数量在合理范围内
    if (length(knots_custom) < 2) {
      knots_custom <- quantile(sorted_data$x, probs = seq(0, 1, length.out = 3))
    }

    # 使用自定义节点拟合COBS模型
    spline_fit <- cobs(
      x = sorted_data$x,
      y = sorted_data$y,
      constraint = "increase",
      knots = knots_custom,  # 使用自定义节点
      degree = 2,
      lambda = 0
    )

    # 生成预测点
    pred_x <- seq(min(processing_data$x), max(processing_data$x), length.out = 1000)
    pred_y <- predict(spline_fit, pred_x)[, 2]

    # 计算导数
    x_points <- seq(min(processing_data$x), max(processing_data$x), length.out = 1000)
    dy_dx_smooth <- smooth_derivative(spline_fit, x_points, h_factor = 0.05)

    # 返回结果
    return(list(
      processing_data = processing_data,
      pred_x = pred_x,
      pred_y = pred_y,
      dy_dx_smooth = dy_dx_smooth,
      x_points = x_points,
      rq_models = rq_models,
      rq_quantiles = rq_quantiles
    ))
  })
}

# 函数，用于计算rq回归对数据分布的校正，并输出作图数据
get_spline_result_data <- function(rq_formula, compare_data, rq_quantiles, indp_elements, baseline, knot_num, i){

  withProgress(message = '正在计算rq_regression', value = 0, {
    # 计算分位数回归模型
    rq_models <- lapply(1:length(rq_quantiles), function(j){
      incProgress(1/length(rq_quantiles), detail = paste("\n 处理组别: ", i, " ", "rq_regression", j, "/", length(rq_quantiles)))

      rq(rq_formula, tau = rq_quantiles[[j]], data = compare_data) %>% suppressWarnings()
    })

    # 创建预测数据
    pred_data <- data.frame(
      tau = unlist(rq_quantiles),
      自变量水平 = indp_elements,
      baseline
    )

    # 获取预测值
    pred_data$predicted_value <- lapply(rq_models, function(model){
      predict(model, newdata = baseline) %>% unlist
    })

    # 处理数据
    processing_data <- pred_data[c("predicted_value", "tau")]
    colnames(processing_data) <- c("x", "y")
    processing_data$x <- unlist(processing_data$x)
    processing_data$y <- unlist(processing_data$y)

    # 添加锚点
    anchor_points <- data.frame(
      x = c(0.00001, 10),
      y = c(0, 1)
    )

    # 合并数据
    data_with_anchors <- rbind(processing_data, anchor_points)

    # 设置约束
    con <- rbind(
      c(0, 0.00001, 0),
      c(0, 10, 1)
    )

    # 确保数据按x排序
    sorted_data <- data_with_anchors[order(data_with_anchors$x), ]

    # 计算每隔4个点的节点位置
    n_points <- nrow(sorted_data)
    knot_indices <- seq(1, n_points, by = 7)

    # 确保包含第一个和最后一个点（重要边界）
    if (!1 %in% knot_indices) knot_indices <- c(1, knot_indices)
    if (!n_points %in% knot_indices) knot_indices <- c(knot_indices, n_points)

    # 获取实际的x值作为节点
    knots_custom <- sorted_data$x[knot_indices] %>% unique() %>% sort()

    # 确保节点数量在合理范围内
    if (length(knots_custom) < 2) {
      knots_custom <- quantile(sorted_data$x, probs = seq(0, 1, length.out = 3))
    }

    # 使用自定义节点拟合COBS模型
    spline_fit <- cobs(
      x = sorted_data$x,
      y = sorted_data$y,
      constraint = "increase",
      knots = knots_custom,  # 使用自定义节点
      degree = 2,
      lambda = 0
    )

    # 生成预测点
    pred_x <- seq(min(processing_data$x), max(processing_data$x), length.out = 1000)
    pred_y <- predict(spline_fit, pred_x)[, 2]

    # 计算导数
    x_points <- seq(min(processing_data$x), max(processing_data$x), length.out = 1000)
    dy_dx_smooth <- smooth_derivative(spline_fit, x_points, h_factor = 0.05)

    # 返回结果
    return(list(
      processing_data = processing_data,
      pred_x = pred_x,
      pred_y = pred_y,
      dy_dx_smooth = dy_dx_smooth,
      x_points = x_points,
      rq_models = rq_models,
      rq_quantiles = rq_quantiles
    ))
  })
}

# 函数，对条样回归作图
spline_plot_function <- function(processing_data, pred_x, pred_y){

  anchor_points <- data.frame(
    x = c(0.00001, 10000),
    y = c(0, 1)
  )

  # 创建基础图形
  p <- plot_ly() %>%
    # 添加原始数据点
    add_trace(
      x = processing_data$x,
      y = processing_data$y,
      type = "scatter",
      mode = "markers",
      marker = list(size = 8, color = "black"),
      name = "数据点",
      hoverinfo = "x+y"
    ) %>%
    # 添加拟合样条线
    add_trace(
      x = pred_x,
      y = pred_y,
      type = "scatter",
      mode = "lines",
      line = list(color = "blue", width = 3),
      name = "拟合样条"
    )  %>%
    # 设置布局
    layout(
      title = "带锚点的三次样条回归",
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      legend = list(
        x = 0.01,
        y = 0.99,
        bgcolor = "rgba(255,255,255,0.7)"
      ),
      hovermode = "closest"
    )

  # 显示图形
  p

}

# 自适应绘图函数
plot_quantile_lines <- function(data, origin_data, best_lambda, header_name, extreme_quantiles) {
  # 获取唯一群体和分位数
  condition <- unique(data$condition)
  quantiles <- unique(data$`CI_%`)
  breaks_transformed <- pretty(origin_data$boxcox_result, 8)
  breaks_original <- round(boxcox_rev(breaks_transformed, best_lambda) - 0.01, 2)
  # 自动生成颜色方案
  group_palette <- c("lightcoral",
                     "#66CCFF",
                     "#9AFF9A",    # 浅绿色 (与深绿色对应)
                     "#FFD700",    # 浅金色 (与深棕色对应)
                     "#DA70D6")    # 浅紫色 (与深紫色对应)

  quantile_palette <- c("brown",
                        "darkblue",
                        "#006400",    # 深绿色 (与浅绿色同色调)
                        "#8B4513",    # 深棕色 (与浅金色同色调)
                        "#800080")    # 深紫色 (与浅紫色同色调)
  color_multi <- ceiling(length(condition)/length(group_palette))
  group_palette <- group_palette %>% rep(color_multi)
  quantile_palette <- quantile_palette %>% rep(color_multi)

  # 创建基础图表
  p <- plot_ly()

  # 第一步：添加原始数据点（散点图）
  for (grp in condition) {
    # 筛选该群体的原始数据
    group_origin <- origin_data %>% filter(.data[[header_name]] == grp)

    if (nrow(group_origin) > 0) {
      group_color <- group_palette[which(condition == grp)]

      # 添加原始数据点
      p <- p %>% add_trace(
        data = group_origin,
        x = ~年龄,
        y = ~boxcox_result,
        type = "scatter",
        mode = "markers",
        name = paste(grp, nrow(group_origin), "例"),
        marker = list(
          color = paste0(
            "rgba(",
            col2rgb(group_color)[1], ",",
            col2rgb(group_color)[2], ",",
            col2rgb(group_color)[3], ",0.3)"
          ), # 半透明
          size = 5
        ),
        hoverinfo = ifelse(input$hover_mode == "constant","text","none"),
        text = ~ paste(
          "群体: ", .data[[header_name]],
          "<br>年龄: ", 年龄, "岁",
          "；性别: ", 性别,
          "<br>临床诊断: ", 临床诊断,
          "<br>定量值: ", round(boxcox_rev(boxcox_result, best_lambda), 2)
        ),
        legendgroup = paste0(grp, "_points"),
        showlegend = TRUE
      )

    }
  }
  # 添加分位数折线（按群体分组）
  for (grp in condition) {
    group_data <- data %>% filter(condition == grp)
    group_color <- group_palette[which(condition == grp)]

    for (q in quantiles) {
      q_data <- group_data %>% filter(`CI_%` == q)
      q_color <- quantile_palette[which(condition == grp)]

      # 添加折线
      p <- p %>% add_trace(
        data = q_data,
        x = ~年龄,
        y = ~point_estimator,
        type = "scatter",
        mode = "lines",
        name = paste(grp, "分位数"),
        hoverinfo = "text",  # 启用自定义悬停文本
        text = ~paste(
          "群体: ", condition,
          " 年龄: ", 年龄, "岁",
          `CI_%`, "分位数: ", round(boxcox_rev(point_estimator, best_lambda), 2)
        ),
        line = list(
          shape = 'spline',
          smoothing = input$smoothing_value, # 平滑度
          color = q_color,
          width = 3 - 4*(abs(50 - as.numeric(gsub("%", "", q))))/100,
          dash = ifelse(which(quantiles == q) %% 2 == 0, "dash", "solid")
        ),
        legendgroup = grp,
        showlegend = which(quantiles == q) == 1 # 只为每个群体的第一条线显示图例
      )

      if (q %in% extreme_quantiles) {
        # 方法1：使用 error_y（垂直误差线）
        p <- p %>% add_trace(
          data = q_data,
          x = ~年龄,
          y = ~point_estimator,
          type = "scatter",
          mode = "markers",
          error_y = list(
            type = "data",
            symmetric = FALSE,
            array = ~CI_upper - point_estimator,
            arrayminus = ~point_estimator - CI_lower,
            color = q_color,
            thickness = 0.5,
            width = 0
          ),
          marker = list(size = 0.1, opacity = 0), # 隐藏点
          showlegend = FALSE,
          legendgroup = grp,
          hoverinfo = "none"
        )
      }
    }
  }

  # 设置布局
  p %>% layout(
    hoverlabel = list(
      font = list(
        size = 16,  # 设置悬浮框字体大小
        family = "Arial"
      ),
      bgcolor = "rgba(255,255,255,0.9)",  # 背景颜色
      bordercolor = "#333",  # 边框颜色
      namelength = -1  # 显示完整名称
    ),
    title = list(
      text = paste0(input$dynamic_header_,"分层分位数趋势/散点图"),
      y = 0.98,  # 向下移动标题位置 (0-1范围，1为顶部)
      x = 0.5,   # 居中
      xanchor = "center",
      yanchor = "top",
      font = list(
        size = 25,  # 标题字体大小
        family = "Arial"
      )
    ),
    xaxis = list(
      title = list(
        text = "年龄 (岁)",
        font = list(
          size = 25,  # x轴标题字体大小
          family = "Arial"
        )
      ),
      tickfont = list(
        size = 25,  # x轴刻度字体大小
        family = "Arial"
      )
    ),
    yaxis = list(
      title = list(
        text = "指标水平",
        font = list(
          size = 25,  # y轴标题字体大小
          family = "Arial"
        )
      ),
      tickfont = list(
        size = 25,  # y轴刻度字体大小
        family = "Arial"
      ),
      tickvals = breaks_transformed,
      ticktext = breaks_original
    ),
    hovermode = input$hover_mode,
    legend = list(
      title = list(
        text = "<b>各病人群体散点/分位数</b>",
        font = list(size = 25)  # 图例标题字体大小
      ),
      font = list(size = 25),  # 图例项字体大小
      itemsizing = "constant",
      traceorder = "grouped"
    ),
    margin = list(
      l = 50,  # 增加左边距给y轴标题更多空间
      r = 20,
      b = 50,  # 增加底部边距给x轴标题更多空间
      t = 50  # 增加顶部边距给主标题更多空间
    )
  )

}

# 函数，用于动态调整回归公式
delete_var <- function(formular, deleted_var) {
  # 输入验证：使用 inherits 更可靠
  if (!inherits(formular, "formula")) {
    stop("formular must be a formula object")
  }
  if (!is.character(deleted_var) || length(deleted_var) != 1) {
    stop("deleted_var must be a single character string")
  }

  # 将公式转换为 terms 对象进行解析
  t <- terms(formular)

  # 获取所有预测变量项（不包括响应变量）
  term_labels <- attr(t, "term.labels")
  if (is.null(term_labels)) term_labels <- character(0)

  # 获取截距信息
  has_intercept <- attr(t, "intercept") == 1

  # 处理每一项：检查是否包含目标变量
  kept_terms <- character(0)
  for (term in term_labels) {
    # 使用 all.vars 提取表达式中的原始变量名
    vars_in_term <- all.vars(as.formula(paste0("~", term)))

    # 如果该项不包含要删除的变量，则保留
    if (!deleted_var %in% vars_in_term) {
      kept_terms <- c(kept_terms, term)
    }
  }

  # 获取响应变量（左侧）
  has_response <- attr(t, "response") == 1
  if (has_response) {
    response <- deparse(formular[[2]], width.cutoff = 500)
  } else {
    response <- NULL
  }

  # 构建新的右侧表达式
  if (length(kept_terms) == 0) {
    predictors <- if (has_intercept) "1" else "0"
  } else {
    predictors <- paste(kept_terms, collapse = " + ")
  }

  # 构造新公式
  if (is.null(response)) {
    new_formula_str <- paste("~", predictors)
  } else {
    new_formula_str <- paste(response, "~", predictors)
  }

  # 返回公式对象
  as.formula(new_formula_str)
}

return_validity_of_variables <- function(data, all_var_to_test){
  validity_of_variables_lst <- lapply(all_var_to_test, function(var_to_test){
    if (is.numeric(data[[var_to_test]]) || is.integer(data[[var_to_test]])){
      validated_range <- c(quantile(data[[var_to_test]], 0.05), quantile(data[[var_to_test]], 0.95))
      names(validated_range) <- c("lower_limit", "upper_limit")
      validated_range
    } else{
      validated_var_lst <- data %>%
        group_by(!!sym(var_to_test)) %>%
        reframe(nrow = n()) %>%
        arrange(desc(nrow)) %>%
        filter(nrow >= 50)

      validated_var_lst[[var_to_test]] %>% unlist %>% list
    }
  })
  names(validity_of_variables_lst) <- all_var_to_test
  validity_of_variables_lst
}


calculate_FWHM <- function(data) {
  # 确保数据按x排序
  data <- data.frame(
    x = data$pred_x,
    y = data$dy_dx_smooth
  )

  data <- data[order(data$x), ]
  x <- data$x
  y <- data$y

  # 归一化y值使其成为概率密度函数(PDF)
  total_area <- sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  y_norm <- y / total_area
  data$y_norm <- y_norm  # 添加归一化列

  # 找到所有峰值（局部最大值）
  peaks <- which(diff(sign(diff(y_norm))) == -2) + 1

  # 如果没有找到峰值，返回NA
  if (length(peaks) == 0) return(list(left = NA, right = NA, midpoint_height = NA, peak_height = NA))

  # 按峰值高度降序排列
  peak_heights <- y_norm[peaks]
  sorted_peaks <- peaks[order(peak_heights, decreasing = TRUE)]

  # 计算累积分布函数(CDF)
  cdf <- cumsum(c(0, (diff(x) * (head(y_norm, -1) + tail(y_norm, -1)) / 2)))

  # 遍历每个峰值（从最高开始）
  for (peak_idx in sorted_peaks) {
    peak_height <- y_norm[peak_idx]
    half_max <- peak_height / 2

    # 在左侧寻找半高宽点
    left_idx <- peak_idx
    while (left_idx > 1 && y_norm[left_idx] > half_max) {
      left_idx <- left_idx - 1
    }

    # 线性插值找到精确的左边界
    if (left_idx < peak_idx) {
      x_left <- approx(
        x = y_norm[c(left_idx, left_idx + 1)],
        y = x[c(left_idx, left_idx + 1)],
        xout = half_max
      )$y
      if (half_max < min(y_norm[c(left_idx, left_idx + 1)])){x_left <- 0}
    } else {
      next  # 跳过无法找到左边界的峰值
    }

    # 在右侧寻找半高宽点
    right_idx <- peak_idx
    while (right_idx < length(y_norm) && y_norm[right_idx] > half_max) {
      right_idx <- right_idx + 1
    }

    # 线性插值找到精确的右边界
    if (right_idx > peak_idx) {
      x_right <- approx(
        x = y_norm[c(right_idx - 1, right_idx)],
        y = x[c(right_idx - 1, right_idx)],
        xout = half_max
      )$y
      if (half_max < min(y_norm[c(right_idx - 1, right_idx)])){x_right <- max(y_norm[c(right_idx - 1, right_idx)])}
    } else {
      next  # 跳过无法找到右边界的峰值
    }

    # 计算FWHM区间内的PDF面积
    idx_range <- which(x >= x_left & x <= x_right)
    area_points <- c(x_left, x[idx_range], x_right)
    y_points <- c(half_max, y_norm[idx_range], half_max)

    # 使用梯形法则计算面积
    fwhm_area <- sum(diff(area_points) * (head(y_points, -1) + tail(y_points, -1)) / 2)

    # 检查面积是否超过35%
    if (fwhm_area >= 0.35) {
      # 计算FWHM中点
      midpoint <- mean(c(x_left, x_right))

      # 计算中点高度
      # 找到中点所在的区间
      mid_idx <- findInterval(midpoint, x)

      # 确保在有效范围内
      if (mid_idx >= 1 && mid_idx < length(x)) {
        # 线性插值公式
        x1 <- x[mid_idx]
        x2 <- x[mid_idx + 1]
        y1 <- y_norm[mid_idx]
        y2 <- y_norm[mid_idx + 1]

        # 插值计算中点高度
        midpoint_height <- y1 + (y2 - y1) * (midpoint - x1) / (x2 - x1)
      } else {
        # 如果中点超出范围，使用最近点
        if (midpoint < min(x)) {
          midpoint_height <- y_norm[1]
        } else if (midpoint > max(x)) {
          midpoint_height <- y_norm[length(y_norm)]
        } else {
          midpoint_height <- NA
        }
      }


      equivalent_height <- fwhm_area/(x_right - x_left)
      FWHM <- x_right - x_left
      # 返回完整结果
      return(
        list(
          left = x_left,
          right = x_right,
          midpoint = midpoint,
          midpoint_height = midpoint_height,
          peak_height = peak_height,
          fwhm_area = fwhm_area,
          equivalent_height = equivalent_height,
          FWHM = FWHM,
          equivalent_height_FWHM_ratio = equivalent_height/FWHM,
          midpoint_height_FWHM_ratio = midpoint_height/FWHM
        )
      )
    }
  }

  # 如果没有峰值满足条件，返回NA
  return(
    list(
      left = NA,
      right = NA,
      midpoint_height = NA,
      peak_height = NA,
      fwhm_area = NA,
      equivalent_height = NA,
      FWHM = NA,
      equivalent_height_FWHM_ratio = NA,
      midpoint_height_FWHM_ratio = NA
    )
  )
}

generate_quantiles_and_rq_models <- function(oringin_data) {
  print(paste0("", oringin_data[["分层名称"]]))

  # 获取参数
  rq_acc <- 0.001
  rq_num <- max(min(ceiling(nrow(oringin_data)/5), 200), 25)

  # 定义非线性变换函数 (Beta分布)
  # 使序列在两端更密集 (shape < 1)
  nonlinear_seq <- function(n, lower, upper, shape = 0.1) {
    u <- seq(0, 1, length.out = n)
    # Beta累积分布函数的反函数 (两端密集)
    q <- qbeta(u, shape, shape)
    # 缩放至目标区间
    lower + q * (upper - lower)
  }

  # 生成序列1：基于定量结果的极端值 (非线性变换)
  n_points <- ceiling(rq_num*1)
  target_range <- c(
    quantile(oringin_data[["数据"]]$定量结果, rq_acc),
    quantile(oringin_data[["数据"]]$定量结果, 1 - rq_acc)
  )

  # 在定量结果范围内生成非线性序列
  dense_x <- nonlinear_seq(n_points, target_range[1], target_range[2])
  seq1 <- unlist(lapply(dense_x, get_quantile_position, data = oringin_data[[1]]$定量结果))

  # 生成序列2：极端分位数位置 (非线性变换)
  seq2 <- nonlinear_seq(n_points, rq_acc, 1 - rq_acc)

  # 合并并去重
  combined <- c(seq1, seq2) %>% unique() %>% sort()
  # showNotification(paste("生成点数:", length(combined)))

  rq_models <- lapply(1:length(combined), function(j){
    rq(oringin_data[["回归公式"]], tau = combined[[j]], data = oringin_data[["数据"]]) %>% suppressWarnings()
  })

  return(list(
    "数据" = oringin_data[["数据"]],  # 回归数据
    "分层名称" = oringin_data[["分层名称"]],  # 颜色组名
    "有效协变量" = oringin_data[["有效协变量"]],
    "回归公式" = oringin_data[["回归公式"]],
    "分位数位置" = combined,
    "rq_models" = rq_models
  ))
}

# 最简实现：计算一个值在数据集中的分位数位置
get_quantile_position <- function(data, value) {
  # 计算小于等于给定值的样本数
  le_count <- sum(data <= value)
  # 计算小于给定值的样本数
  lt_count <- sum(data < value)
  # 总样本数
  n <- length(data)
  # 使用标准分位数定义 (Type 7，R默认)
  position <- (le_count + lt_count) / (2 * n)
  return(position)
}

# 筛选所有当前基线水平下活跃的分层
filter_active_stratifications_data <- function(baseline, data_to_cal){
  for (i in colnames(baseline)) {

    for (j in seq_along(data_to_cal)){

      if (data_to_cal[[j]] %>% is.na %>% all){
        next
      }

      baseline_level <- baseline[[i]] %>% unlist

      testing_level <- data_to_cal[[j]][["有效协变量"]][[i]] %>% unlist

      if (all(is.numeric(testing_level))){
        test_result <-  (baseline_level >= testing_level[["lower_limit"]] & baseline_level <= testing_level[["upper_limit"]])
      }else{
        test_result <- (baseline_level %in% testing_level)
      }
      if (!test_result){
        data_to_cal[[j]] <- NA
      }
    }
  }
  location <- which(!is.na(data_to_cal))

  data_to_cal[location]
}

smooth_derivative <- function(fit, x_points, h_factor = 0.001, shrink_factor = 0.1) {
  # 使用四分位距(IQR)标准化步长
  x_iqr <- IQR(x_points, na.rm = TRUE)
  h_base <- h_factor * x_iqr

  # 确定前0.2%区域的边界
  x_sorted <- sort(x_points)
  threshold <- x_sorted[ceiling(0.002 * length(x_sorted))]

  # 计算自适应步长向量
  h_values <- ifelse(x_points < threshold, shrink_factor * h_base, h_base)

  # 初始化导数向量
  dy_dx <- numeric(length(x_points))

  # 计算中心差分
  for (i in seq_along(x_points)) {
    x <- x_points[i]
    h <- h_values[i]

    # 边界保护
    x_left <- max(x - h, min(x_points))
    x_right <- min(x + h, max(x_points))

    # 使用平滑预测
    y_left <- predict(fit, x_left)[, 2]
    y_right <- predict(fit, x_right)[, 2]

    dy_dx[i] <- abs((y_right - y_left) / (x_right - x_left))
  }

  return(dy_dx)
}

spline_regression_and_smooth_derivative <- function(data, baseline){
  rq_quantiles <- data[["分位数位置"]]
  rq_models <- data[["rq_models"]]

  # 获取预测值
  predicted_value <- lapply(rq_models, function(model){
    predict(model, newdata = baseline) %>% unlist
  })

  # 创建预测数据
  pred_data <- data.frame(
    "tau" = unlist(rq_quantiles) %>% unname(),
    "predicted_value" = predicted_value %>% unlist %>% unname()
  )
  # 处理数据
  processing_data <- pred_data[c("predicted_value", "tau")]
  colnames(processing_data) <- c("x", "y")
  processing_data$x <- unlist(processing_data$x)
  processing_data$y <- unlist(processing_data$y)
  # 添加锚点
  anchor_points <- data.frame(
    x = c(0.00001, 10),
    y = c(0, 1)
  )
  # 合并数据
  data_with_anchors <- rbind(processing_data, anchor_points)
  # 设置约束
  con <- rbind(
    c(0, 0.00001, 0),
    c(0, 10, 1)
  )
  # 确保数据按x排序
  sorted_data <- data_with_anchors[order(data_with_anchors$x), ]
  # 计算每隔4个点的节点位置
  n_points <- nrow(sorted_data)
  knot_indices <- seq(1, n_points, by = 7)
  # 确保包含第一个和最后一个点（重要边界）
  if (!1 %in% knot_indices) knot_indices <- c(1, knot_indices)
  if (!n_points %in% knot_indices) knot_indices <- c(knot_indices, n_points)
  # 获取实际的x值作为节点
  knots_custom <- sorted_data$x[knot_indices] %>% unique() %>% sort()
  # 确保节点数量在合理范围内
  if (length(knots_custom) < 2) {
    knots_custom <- quantile(sorted_data$x, probs = seq(0, 1, length.out = 3))
  }
  # 使用自定义节点拟合COBS模型
  spline_fit <- cobs(
    x = sorted_data$x,
    y = sorted_data$y,
    constraint = "increase",
    knots = knots_custom,  # 使用自定义节点
    degree = 2,
    lambda = 0
  )
  # 生成预测点
  pred_x <- seq(min(processing_data$x), max(processing_data$x), length.out = 1000)
  pred_y <- predict(spline_fit, pred_x)[, 2]
  # 计算导数
  x_points <- seq(min(processing_data$x), max(processing_data$x), length.out = 1000)
  dy_dx_smooth <- smooth_derivative(spline_fit, x_points, h_factor = 0.05)
  # 返回结果
  return(list(
    processing_data = processing_data,
    pred_x = pred_x,
    pred_y = pred_y,
    dy_dx_smooth = dy_dx_smooth,
    x_points = x_points,
    rq_models = rq_models,
    rq_quantiles = rq_quantiles
  ))
}

all_pairs <- function(vec) {
  # 输入验证
  if (!is.vector(vec)) stop("输入必须是向量")
  if (any(duplicated(vec))) stop("向量包含重复元素")

  n <- length(vec)
  # 处理边界情况
  if (n < 2) {
    message("向量长度小于2，无法形成组合")
    return(data.frame(Var1 = character(0), Var2 = character(0)))
  }

  # 计算组合数量
  num_combinations <- choose(n, 2)

  # 创建结果数据框
  result <- data.frame(
    Var1 = rep(NA, num_combinations),
    Var2 = rep(NA, num_combinations),
    stringsAsFactors = FALSE
  )

  # 填充组合
  idx <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      result$Var1[idx] <- vec[i]
      result$Var2[idx] <- vec[j]
      idx <- idx + 1
    }
  }

  return(result)
}

# 计算box-cox最佳λ - 完全避免环境问题
# 直接使用数值向量而不是数据框
maxlikelihood_boxcox <- function(data_vector){

  y_values <- as.numeric(data_vector) + 0.1
  # 手动实现Box-Cox优化
  lambda_seq <- seq(-2, 2, by = 0.05)
  log_likelihoods <- sapply(lambda_seq, function(lambda) {
    if (lambda == 0) {
      y_trans <- log(y_values)
    } else {
      y_trans <- (y_values^lambda - 1) / lambda
    }

    # 计算对数似然
    n <- length(y_trans)
    var_y <- var(y_trans)
    if (is.na(var_y)) return(-Inf)
    if (var_y == 0) return(-Inf)  # 避免零方差

    -n/2 * log(var_y) + (lambda - 1) * sum(log(y_values))
  })

  # 找到最佳lambda
  best_idx <- which.max(log_likelihoods)
  best_lambda <- lambda_seq[best_idx]

  # 利用最优λ进行正态转换
  data_vector_return <- if (best_lambda == 0) {
    log(as.numeric(data_vector) + 0.1)
  } else {
    ((as.numeric(data_vector) + 0.1)^best_lambda - 1) / best_lambda
  }
  data_vector_return
}

calculate_SDR <- function(grouped_data) {
  # 合并所有数据
  rbind_data <- do.call(rbind, grouped_data)
  total_sd <- sd(rbind_data$定量结果)

  # 计算各组均值的标准差（更直接的“组间标准差”）
  group_means <- sapply(grouped_data, function(x) mean(x$定量结果))
  sd_between_groups <- sd(group_means)

  # 返回原始SDR和方差占比
  return(sd_between_groups / total_sd)
}

# 计算两组间Cohen's d效应量
cohens_d <- function(group1, group2) {
  # 输入验证
  if (length(group1) < 2 || length(group2) < 2) {
    stop("每组至少需要2个观测值")
  }

  # 计算组统计量
  n1 <- length(group1)
  n2 <- length(group2)
  mean1 <- mean(group1)
  mean2 <- mean(group2)
  sd1 <- sd(group1)
  sd2 <- sd(group2)

  # 计算合并标准差
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

  # 计算Cohen's d
  d <- (mean1 - mean2) / pooled_sd

  return(d)
}

cut_data_age <- function(data, cut_points, SDR = 1){

  cut_points <- c(min(data$年龄), cut_points, max(data$年龄))
  return_data <- lapply(1:(length(cut_points) - 1), function(i, data){
    data %>% filter(年龄 >= cut_points[[i]], 年龄 < cut_points[[i + 1]])
  },
  data = data)
  names(return_data) <- lapply(1:(length(cut_points) - 1), function(i){
    paste0(cut_points[[i]],"~",cut_points[[i + 1]]-1)
  }) %>% unlist

  return_data
}

generate_segments <- function(n, m, x, mini_distance = 5) {
  # 处理无效输入
  if (n > m || x < 0 || mini_distance < 1) {
    return(list())
  }

  # 若无需分段点
  if (x == 0) {
    return(list(integer(0)))
  }

  # 计算年龄范围
  age_range <- m - n + 1

  # 如果可选点数不足
  if (age_range < x) {
    return(list())
  }

  # 生成所有可能的组合
  age_sequence <- n:m
  combinations <- combn(age_sequence, x, simplify = FALSE)

  # 如果不需要最小间隔，直接返回所有组合
  if (mini_distance == 1) {
    return(combinations)
  }

  # 过滤函数：检查组合是否满足最小间隔要求
  satisfies_mini_distance <- function(combination) {
    combination_ <- c(n,combination,m)

    # 如果只有一个点，总是满足条件
    if (length(combination_) == 2) {
      return(TRUE)
    }

    # 检查所有相邻点之间的间隔
    for (i in 1:(length(combination_) - 1)) {
      if (combination_[i + 1] - combination_[i] < mini_distance) {
        return(FALSE)
      }
    }
    return(TRUE)
  }

  # 应用过滤函数
  filtered_combinations <- Filter(satisfies_mini_distance, combinations)

  return(filtered_combinations)
}

series_of_segment <- function(data, segment_num, mini_distance){
  min_age <- min(data$年龄) %>% na.omit()
  max_age <- max(data$年龄) %>% na.omit()
  # 初始切割位置
  all_possible_cut_points <- generate_segments(min_age, max_age, segment_num, mini_distance)

  return(all_possible_cut_points)
}

split_by_sex <- function(data){
  male <- data %>% filter(性别 == "男")
  female <- data %>% filter(性别 == "女")
  return(list("Male" = male, "Female" = female))
}

# 对数据进行所有可能的分段点的年龄、性别分段，并计算SDR
return_SDR_lst <- function(all_possible_cut_points, data, indicator_names){
  # 先进行正态转换
  data$定量结果 <- maxlikelihood_boxcox(data$定量结果)
  sex_stra_status <- list()
  SDR_list <- list()


  for (i in seq_along(all_possible_cut_points)){ # 遍历所有可能的分段点
    if (i %% 100 == 0){print(paste0(indicator_names, " ", i))} # 每100个点print一下，方便跟进计算进度

    sex_stra_status[[i]] <- list()
    cut_points <- all_possible_cut_points[[i]] # 提取本数据所有可能的分段点
    cut_data <- cut_data_age(data, cut_points) # 对本数据进行年龄分割

    for (j in seq_along(cut_data)){ # 判断每个层级是否需要年龄分段
      male <- cut_data[[j]] %>% filter(性别 == "男")
      female <- cut_data[[j]] %>% filter(性别 == "女")
      sex_SDR <- calculate_SDR(list(male, female))
      if (is.na(sex_SDR)){
        sex_stra_status[[i]][[j]] <- FALSE
      }else if (sex_SDR > 0.3){
        sex_stra_status[[i]][[j]] <- TRUE
      } else{sex_stra_status[[i]][[j]] <- FALSE}
    }

    if (any(sex_stra_status[[i]] %>% unlist)){ # 如需要年龄分段，则进行年龄分层，并计算SDR
      group_num_to_split <- which(sex_stra_status[[i]] %>% unlist)
      add_df <- lapply(ifelse(length(group_num_to_split)>1,cut_data[group_num_to_split],cut_data[group_num_to_split]), split_by_sex)[[1]]
      cut_data <- c(cut_data[-group_num_to_split], add_df)
    }

    if (any(lapply(cut_data, nrow) %>% unlist < 1000)){ # 如果任何分层样本量小于1000,则放弃该分层方案
      SDR_list[[i]] <- NA
      next
    }

    SDR_list[[i]] <- calculate_SDR(cut_data)
  }
  return(
    list(
      "age_cutoff_points" = all_possible_cut_points,
      "corresponding_SDR" = SDR_list,
      "sex_stratification_status" = sex_stra_status
    )
  )
}

# 函数返回最优SDR


return_best_SDR <- function(stra_result_lst){
  age_cutoff_points <- list()
  corresponding_SDR <- list()
  sex_stratification_status <- list()
  for (stra_result in stra_result_lst){
    age_cutoff_points <- c(age_cutoff_points, stra_result[["age_cutoff_points"]])
    corresponding_SDR <- c(corresponding_SDR, stra_result[["corresponding_SDR"]])
    sex_stratification_status <- c(sex_stratification_status, stra_result[["sex_stratification_status"]])
  }

  age_cutoff_points_lst <- lapply(seq_along(age_cutoff_points), function(i){
    (age_cutoff_points[[i]] %>% length) + (sex_stratification_status[[i]] %>% unlist %>% which %>% length)
  })
  num_cut_points <- age_cutoff_points_lst %>% unique %>% unlist %>% sort(decreasing = TRUE)

  for (i in num_cut_points){
    current_SDR_lst <- corresponding_SDR[which(age_cutoff_points_lst == i)]
    max_SDR <- current_SDR_lst %>% unlist %>% na.omit %>% max
    if (max_SDR >= 0.2){
      max_SDR_location <- which(corresponding_SDR %>% unlist == max_SDR)[[1]]
      return(
        list(
          "best_age_cutoff_point" = age_cutoff_points[[max_SDR_location]],
          "best_SDR" = corresponding_SDR[[max_SDR_location]],
          "sex_stratification_status" = sex_stratification_status[[max_SDR_location]]
        )
      )

    }
  }

  max_SDR_location <- 1

  return(
    list(
      "best_age_cutoff_point" = age_cutoff_points[[max_SDR_location]],
      "best_SDR" = corresponding_SDR[[max_SDR_location]],
      "sex_stratification_status" = sex_stratification_status[[max_SDR_location]]
    )
  )
}


statify_data <- function(data, best_stratified_data_by_SDR){
  result_set1 <- data %>% cut_data_age(., best_stratified_data_by_SDR[["best_age_cutoff_point"]] %>% unlist)
  sex_stratification_status <- best_stratified_data_by_SDR[["sex_stratification_status"]]
  best_SDR <- best_stratified_data_by_SDR[["best_SDR"]]

  result_set2 <- list()
  for (i in seq_along(result_set1)){
    if (sex_stratification_status[[i]]){
      result_set2[[names(result_set1)[[i]]]] <- split_by_sex(result_set1[[i]])
    }else{
      result_set2[[names(result_set1)[[i]]]] <- list("Male&Female" = result_set1[[i]])
    }
  }
  result_set2
}


