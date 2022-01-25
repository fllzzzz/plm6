import request from '@/utils/request'

export function getMachinePart(params) {
  return request({
    module: 'mes',
    url: 'wageQuota/machine_part/page',
    method: 'get',
    params
  })
}

//修改工序工价
export function processWageQuotaSave(data) {
  return request({
    module: 'mes',
    url: 'process/wage',
    method: 'post',
    data
  })
}
