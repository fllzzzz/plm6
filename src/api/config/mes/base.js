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
 * @param {number} id 用户id
 * @param {string} phone 用户手机号
 * @param {number} maxWeight 过磅超标重量允许值
 */
export function setOverweightSMSRecipient({ id, phone, maxWeight }) {
  return request({
    url: 'api/config/mes/overweightSMSRecipient',
    method: 'put',
    data: { id, phone, maxWeight }
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
