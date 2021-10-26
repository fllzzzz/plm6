import request from '@/utils/request'

// 获取入库基础配置
export function getInboundBasicConf() {
  return request({
    module: 'wms',
    url: 'config/inbound/base',
    method: 'get'
  })
}

// 保存入库基础配置
export function setInboundBasicConf(params) {
  return request({
    module: 'wms',
    url: 'config/inbound/base',
    method: 'put',
    params
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
export function setInboundSteelConf(params) {
  return request({
    module: 'wms',
    url: 'config/inbound/steel',
    method: 'put',
    params
  })
}
