import request from '@/utils/request'

// 获取wms配置
export function getWmsConfig() {
  return request({
    module: 'wms',
    url: 'config/all',
    method: 'get'
  })
}

// 获取入库基础配置
export function getInboundBasicConf() {
  return request({
    module: 'wms',
    url: 'config/inbound/base',
    method: 'get'
  })
}

// 保存入库基础配置
export function setInboundBasicConf(data) {
  return request({
    module: 'wms',
    url: 'config/inbound/base',
    method: 'put',
    data
  })
}

// 获取入库钢材配置
export function getInboundSteelConf() {
  return request({
    module: 'wms',
    url: 'config/inbound/steel',
    method: 'get'
  })
}

// 保存入库钢材配置
export function setInboundSteelConf(data) {
  return request({
    module: 'wms',
    url: 'config/inbound/steel',
    method: 'put',
    data
  })
}

// 获取报表中心配置
export function getReportCenterConf() {
  return request({
    module: 'wms',
    url: 'config/report/download',
    method: 'get'
  })
}

// 保存报表中心配置
export function setReportCenterConf(data) {
  return request({
    module: 'wms',
    url: 'config/report/download',
    method: 'put',
    data
  })
}

// 获取出库基础配置
export function getOutboundBasicConf() {
  return request({
    module: 'wms',
    url: 'config/outbound/base',
    method: 'get'
  })
}

// 保存出库基础配置
export function setOutboundBasicConf(data) {
  return request({
    module: 'wms',
    url: 'config/outbound/base',
    method: 'put',
    data
  })
}

// 获取甲供材料归还基础配置
export function getPartyABorrowReturnConf() {
  return request({
    module: 'wms',
    url: 'config/transfer/party-a-borrow-return',
    method: 'get'
  })
}

// 保存甲供材料归还基础配置
export function setPartyABorrowReturnConf(data) {
  return request({
    module: 'wms',
    url: 'config/transfer/party-a-borrow-return',
    method: 'put',
    data
  })
}

// 获取物料加权配置
export function getMaterialWeightingConf() {
  return request({
    module: 'wms',
    url: 'config/weighted-price',
    method: 'get'
  })
}

// 保存物料加权配置
export function setMaterialWeightingConf(data) {
  return request({
    module: 'wms',
    url: 'config/weighted-price',
    method: 'put',
    data
  })
}

// 获取退货基础配置
export function getRejectBasicConf() {
  return request({
    module: 'wms',
    url: 'config/reject/base',
    method: 'get'
  })
}

// 保存退货基础配置
export function setRejectBasicConf(data) {
  return request({
    module: 'wms',
    url: 'config/reject/base',
    method: 'put',
    data
  })
}

// 获取退库审核配置
export function getReturnBasicConf() {
  return request({
    module: 'wms',
    url: 'config/return',
    method: 'get'
  })
}

// 保存退库审核配置
export function setReturnBasicConf(data) {
  return request({
    module: 'wms',
    url: 'config/return',
    method: 'put',
    data
  })
}

// 获取调拨审核配置
export function getTransferBasicConf() {
  return request({
    module: 'wms',
    url: 'config/transfer',
    method: 'get'
  })
}

// 保存调拨审核配置
export function setTransferBasicConf(data) {
  return request({
    module: 'wms',
    url: 'config/transfer',
    method: 'put',
    data
  })
}

// 获取废料价格录入配置
export function getScrapPriceConf() {
  return request({
    url: 'api/config/getWastePriceEntryType',
    method: 'get'
  })
}

// 保存废料价格录入配置
export function setScrapPriceConf(params) {
  return request({
    url: 'api/config/setWastePriceEntryType',
    method: 'put',
    params
  })
}
