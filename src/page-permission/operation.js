// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 运营分析 start ------------------------------

// 生产成本分析
export const productionCostAnalysisPM = {
  get: ['production_cost_analysis:get'], // 列表
  download: ['production_cost_analysis:download'] // 下载
}

// 涂装费
export const paintingFeeAnalysisPM = {
  get: ['painting_fee_analysis:get'], // 列表
  download: ['painting_fee_analysis:download'] // 下载
}

// 辅材费
export const auxiliaryFeeAnalysisPM = {
  get: ['auxiliary_fee_analysis:get'], // 列表
  download: ['auxiliary_fee_analysis:download'] // 下载
}

// 检测费
export const testingFeeAnalysisPM = {
  get: ['operation_testing:get'], // 列表
  download: ['operation_testing:download'] // 下载
}

// 水电
export const operationWaterElectricityPM = {
  get: ['operation_water_electricity:get'], // 列表
  download: ['operation_water_electricity:download'] // 下载
}

// 管理费
export const operationManagementPM = {
  get: ['operation_management:get'], // 列表
  download: ['operation_management:download'] // 下载
}

// 产量分析
export const operationYieldAnalysisPM = {
  get: ['operation_yield_analysis:get'] // 列表
}

// 差异分析
export const operationDifferenceAnalysisPM = {
  get: ['operation_difference_analysis:get'] // 列表
}

// 采购指数
export const purchasingAnalysisPM = {
  get: ['purchasing_analysis:get'] // 列表
}

// QHSE
export const QHSEAnalysisPM = {
  get: ['QHSE_analysis:get'] // 列表
}

// QHSE
export const productionTypeAnalysisPM = {
  get: ['production_type_analysis:get'] // 列表
}
// --------------------------- 运营分析 end --------------------------------
