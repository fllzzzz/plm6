import request from '@/utils/request'

export function getMachinePart(params) {
  return request({
    module: 'mes',
    url: 'wageQuota/machine_part/page',
    method: 'get',
    params
  })
}
