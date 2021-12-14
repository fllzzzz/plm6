import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'project/listContracts',
    method: 'get',
    params
  })
}

export default { get }