import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/cargoListLedger',
    method: 'get',
    params
  })
}

export default { get }
