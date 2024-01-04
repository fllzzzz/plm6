
import request from '@/utils/request'

// 获取钢材废料定义
export function getSteelScrapDefinitionConf() {
  return request({
    module: 'wms',
    url: 'config/steel/scrap-definition',
    method: 'get'
  })
}

// 设置入库废料定义
export function setSteelScrapDefinitionConf(data) {
  return request({
    module: 'wms',
    url: 'config/steel/scrap-definition',
    method: 'put',
    data
  })
}

// 无权限获取废料定义
export function getSteelScrapDefinitionConfCommon() {
  return request({
    module: 'wms',
    url: 'config/steel/scrap-definition/getSteelScrapDefinitionConf',
    method: 'get'
  })
}
