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
