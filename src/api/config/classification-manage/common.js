
import request from '@/utils/request'

// 获取科目信息
export function getFinalMatClsById(id) {
  return request({
    module: 'config',
    url: `classification/final-material/${id}`,
    method: 'get'
  })
}
