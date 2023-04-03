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

// 获取采购基础配置
export function getPurchaseBasicConf() {
  return request({
    module: 'wms',
    url: 'config/purchase-spec/base',
    method: 'get'
  })
}

// 保存采购基础配置
export function setPurchaseBasicConf(data) {
  return request({
    module: 'wms',
    url: 'config/purchase-spec/base',
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
