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
