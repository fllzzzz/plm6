
import request from '@/utils/request'

// 获取科目信息
export function getFinalMatClsById(id) {
  return request({
    module: 'config',
    url: `classification/final-material/${id}`,
    method: 'get',
    // cancelKey: `classification/final-material/${id}`
    cancelKey: false
  })
}

// 根据基础物料分类查询科目
export function getMatClsByCls(basicClass) {
  return request({
    module: 'config',
    url: `classification/${basicClass}`,
    method: 'get',
    cancelKey: false
  })
}
