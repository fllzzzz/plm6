import request from '@/utils/request'

/**
 * 获取过磅超标短信接收人
 */
export function getOverweightSMSRecipient() {
  return request({
    url: 'api/config/mes/overweightSMSRecipient',
    method: 'get'
  })
}

/**
 * 设置过磅超标短信接收人
 * @param {array} userChecks 列表信息
 * @param {number} maxWeight 过磅超标重量允许值
 */
export function setOverweightSMSRecipient(data) {
  return request({
    url: 'api/config/mes/overweightSMSRecipient',
    method: 'put',
    data
  })
}

/**
 * 获取安全余额系数
 */
export function getSafetyFactor() {
  return request({
    url: 'api/config/safety/factor',
    method: 'get'
  })
}

/**
 * 设置安全余额系数
 * @param {number} safetyFactor 安全系数
 */
export function setSafetyFactor({ safetyFactor }) {
  return request({
    url: 'api/config/safety/factor',
    method: 'put',
    data: { safetyFactor }
  })
}

/**
 * 获取安装审核配置
 */
export function getInstallationStatus() {
  return request({
    url: 'api/config/installationAudit',
    method: 'get'
  })
}

/**
 * 设置安装审核配置
 * @param {boolean} installationAudit 是否需要安装审核
 */
export function setInstallationStatus(data) {
  return request({
    url: 'api/config/installationAudit',
    method: 'put',
    data
  })
}

/**
 * 获取司机信息填写配置
 */
export function getDriverConfig() {
  return request({
    url: '/api/config/driver',
    method: 'get'
  })
}

/**
 * 设置司机信息填写配置
 */
export function setDriverConfig(data) {
  return request({
    url: '/api/config/driver',
    method: 'put',
    data
  })
}

/**
 * 获取图纸识别规则
 */
export function getDrawingConfig() {
  return request({
    url: '/api/drawing/rules',
    method: 'get'
  })
}

/**
 * 设置图纸识别规则
 */
export function setDrawingConfig(data) {
  return request({
    url: '/api/drawing/rules',
    method: 'post',
    data
  })
}

/**
 * 获取app-打印配置
 */
export function getPrintConfig() {
  return request({
    url: '/api/config/print',
    method: 'get'
  })
}

/**
 * 设置app-打印配置
 */
export function setPrintConfig(data) {
  return request({
    url: '/api/config/print',
    method: 'put',
    data
  })
}

/**
 * 获取车型配置
 */
export function getCarModelConfig() {
  return request({
    url: '/api/config/car/model',
    method: 'get'
  })
}

/**
 * 设置车型配置
 */
export function setCarModelConfig(data) {
  return request({
    url: '/api/config/car/model',
    method: 'put',
    data
  })
}

/**
 * 获取围护-折边件工资计价方式
 */
export function getFoldingPriceMethod() {
  return request({
    url: '/api/config/flanging_piece_price',
    method: 'get'
  })
}

/**
 * 设置围护-折边件工资计价方式
 */
export function setFoldingPriceMethod(data) {
  return request({
    url: '/api/config/flanging_piece_price',
    method: 'put',
    data
  })
}

/**
 * 获取围护配置状态
 */
export function getTechnicalType() {
  return request({
    module: 'config',
    url: 'getTechnicalType',
    method: 'get'
  })
}

/**
 * 设置围护配置状态
 */
export function setTechnicalType(data) {
  return request({
    module: 'config',
    url: 'setTechnicalType',
    method: 'put',
    data
  })
}

/**
 * 获取绑定钢板配置的状态
 */
export function getSteelType() {
  return request({
    module: 'config',
    url: 'getSteelPlate',
    method: 'get'
  })
}

/**
 * 设置绑定钢板配置的状态
 */
export function setSteelType(data) {
  return request({
    module: 'config',
    url: 'setSteelPlate',
    method: 'put',
    data
  })
}
/**
 * 获取APP任务上报重量是否显示
 */
export function getTaskReport() {
  return request({
    module: 'config',
    url: 'getWeight',
    method: 'get'
  })
}

/**
 * 设置APP任务上报重量是否显示
 */
export function setTaskReport(data) {
  return request({
    module: 'config',
    url: 'setWeight',
    method: 'put',
    data
  })
}

/**
 * 获取车间、产线、班组是否显示
 */
export function getInfo() {
  return request({
    module: 'config',
    url: 'show/need',
    method: 'get'
  })
}

/**
 * 设置车间、产线、班组是否显示
 */
export function setInfo({ type }) {
  return request({
    module: 'config',
    url: `show/need/${type}`,
    method: 'put'
  })
}

// 获取构件部件特征定义审批配置
export function getAuditConfig() {
  return request({
    module: 'config',
    url: `getFeatureDefinitionConfig`,
    method: 'get'
  })
}

// 更改构件部件特征定义审批配置
export function setAuditConfig(data) {
  return request({
    module: 'config',
    url: `setFeatureDefinitionConfig`,
    method: 'put',
    data
  })
}

// 获取构件部件特征定义审批配置
export function getPriceConfig() {
  return request({
    module: 'config',
    url: `getPriceEditMode`,
    method: 'get'
  })
}

// 更改构件部件特征定义审批配置
export function setPriceConfig(data) {
  return request({
    module: 'config',
    url: `editPriceEditMode`,
    method: 'post',
    data
  })
}
