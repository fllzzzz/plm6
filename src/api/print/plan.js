import request from '@/utils/request'

export function auxiliaryMaterialSummary(params) {
  return request({
    module: 'plan',
    url: `auxiliaryPart/summary/print`,
    method: 'get',
    params
  })
}

export default {
  auxiliaryMaterialSummary // 自制收货记录
}
