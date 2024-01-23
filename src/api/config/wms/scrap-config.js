import request from '@/utils/request'

// 获取废料配置列表
export function getScrapConfig() {
  return request({
    module: 'wms',
    url: 'config/wasteClassification',
    method: 'get'
  })
}
