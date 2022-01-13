// 配置管理
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {

}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 基础配置 start ------------------------------

// 基础配置/工厂管理
export const configFactoryPM = {
  get: ['factory:get'], // 工厂列表
  add: ['factory:add'], // 添加工厂
  edit: ['factory:edit'], // 编辑工厂
  del: ['factory:del'], // 删除工厂
  editStatus: ['factory:editStatus'] // 更改工厂状态
}

// 基础配置/工厂管理：车间
export const configWorkshopPM = {
  get: ['workshop:get'], // 车间列表
  add: ['workshop:add'], // 添加车间
  edit: ['workshop:edit'], // 编辑车间
  del: ['workshop:del'], // 删除车间
  editStatus: ['workshop:editStatus'] // 更改车间状态
}

// ---------------------------- 基础配置 end -------------------------------

// ########################################################################

// --------------------------- MES-配置管理 start ------------------------------

// MES-配置管理/基础配置
export const configMesBasePM = {
  overweightSMSRecipientGet: ['overweight_sms_recipient:get'], // 查看过磅短信接收人
  overweightSMSRecipientEdit: ['overweight_sms_recipient:edit'], // 编辑过磅短信接收人
  safeAmountFactorGet: ['safe_amount_factor:get'], // 查看安全余额系数
  safeAmountFactorEdit: ['safe_amount_factor:edit'], // 编辑安全余额系数
  driverFillConfigGet: ['driver_fill_config:get'], // 查看物流信息填写配置
  driverFillConfigEdit: ['driver_fill_config:edit'], // 编辑物流信息填写配置
  carModelConfigGet: ['car_model_config:get'], // 查看车型配置
  carModelConfigEdit: ['car_model_config:edit'] // 编辑车型配置
}

// ---------------------------- 基础配置 end -------------------------------

// ########################################################################

// --------------------------- 建钢-生产配置 start ------------------------------

// 建钢-生产配置/生产线管理
export const configProductionLinePM = {
  get: ['productionLine:get'], // 生产线列表
  add: ['productionLine:add'], // 添加生产线
  edit: ['productionLine:edit'], // 编辑生产线
  del: ['productionLine:del'], // 删除生产线
  editStatus: ['productionLine:editStatus'] // 更改生产线状态
}

// 建钢-生产配置/生产线管理:班组
export const configProductionLineTeamPM = {
  get: ['productionLine_team:get'], // 班组列表
  add: ['productionLine_team:add'], // 添加班组
  edit: ['productionLine_team:edit'], // 编辑班组
  del: ['productionLine_team:del'] // 删除班组
}

// 建钢-生产配置/生产线管理:质检
export const configProductionLineInspectPM = {
  get: ['productionLine_inspect:get'], // 质检列表
  add: ['productionLine_inspect:add'], // 添加质检
  edit: ['productionLine_inspect:edit'], // 编辑质检
  del: ['productionLine_inspect:del'] // 删除质检
}

// 建钢-生产配置/工序配置
export const configProcessPM = {
  get: ['process:get'], // 工序列表
  add: ['process:add'], // 添加工序
  edit: ['process:edit'], // 编辑工序
  del: ['process:del'] // 删除工序
}

// 建钢-生产配置/工序管理
export const configProductProcessPM = {
  get: ['product_process:get'], // 产品工序列表
  add: ['product_process:add'], // 添加产品工序
  edit: ['product_process:edit'], // 编辑产品工序
  del: ['product_process:del'], // 删除产品工序
  editStatus: ['product_process:editStatus'] // 更改产品工序状态
}

// 建钢-生产配置/工价定额
export const configWageQuotaPM = {
  get: ['wage_quota:get'], // 工价定额列表
  edit: ['wage_quota:edit'] // 编辑工价定额
}

// ---------------------------- 基础配置 end -------------------------------

// ########################################################################