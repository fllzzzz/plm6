import request from '@/utils/request'

// 获取废料配置列表
export function getScrapConfig(params) {
  return request({
    module: 'wms',
    url: 'config/wasteClassification',
    method: 'get',
    params
  })
}

// 新增废料类型
export function addScrapType(data) {
  return request({
    module: 'wms',
    url: 'config/wasteClassification',
    method: 'post',
    data
  })
}

// 修改废料类型
export function editScrapType(data) {
  return request({
    module: 'wms',
    url: 'config/wasteClassification',
    method: 'put',
    data
  })
}

// 删除废料类型
export function delScrapType(data) {
  return request({
    module: 'wms',
    url: 'config/wasteClassification',
    method: 'delete',
    data
  })
}
