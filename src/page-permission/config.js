// 配置管理
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {

}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 基础配置 start ------------------------------

// 基础配置/工厂管理
export const configFactoryPM = {
  get: ['factory:get'],
  add: ['factory:add'],
  edit: ['factory:edit'],
  del: ['factory:del'],
  editStatus: ['factory:editStatus'] // 更改状态
}

// 基础配置/工厂管理：车间
export const configWorkshopPM = {
  get: ['workshop:get'],
  add: ['workshop:add'],
  edit: ['workshop:edit'],
  del: ['workshop:del'],
  editStatus: ['workshop:editStatus'] // 更改状态
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
  carModelConfigEdit: ['car_model_config:edit']// 编辑车型配置
}

// ---------------------------- 基础配置 end -------------------------------

// ########################################################################

// --------------------------- 建钢-生产配置 start ------------------------------

// 建钢-生产配置/生产线管理
export const configProductionLinePM = {
  get: ['productionLine:get'],
  add: ['productionLine:add'],
  edit: ['productionLine:edit'],
  del: ['productionLine:del'],
  editStatus: ['productionLine:editStatus'] // 更改状态
}

// 建钢-生产配置/生产线管理:班组
export const configProductionLineTeamPM = {
  get: ['productionLine_team:get'],
  add: ['productionLine_team:add'],
  edit: ['productionLine_team:edit'],
  del: ['productionLine_team:del']
}

// 建钢-生产配置/生产线管理:质检
export const configProductionLineInspectPM = {
  get: ['productionLine_inspect:get'],
  add: ['productionLine_inspect:add'],
  edit: ['productionLine_inspect:edit'],
  del: ['productionLine_inspect:del']
}

// 建钢-生产配置/工序配置
export const configProcessPM = {
  get: ['process:get'],
  add: ['process:add'],
  edit: ['process:edit'],
  del: ['process:del']
}

// 建钢-生产配置/工序管理
export const configProductProcessPM = {
  get: ['product_process:get'],
  add: ['product_process:add'],
  edit: ['product_process:edit'],
  del: ['product_process:del'],
  editStatus: ['product_process:editStatus']
}

// 建钢-生产配置/工价定额
export const configWageQuotaPM = {
  get: ['wage_quota:get'],
  edit: ['wage_quota:edit']
}

// ---------------------------- 基础配置 end -------------------------------

// ########################################################################

