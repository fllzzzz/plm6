import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/subLedger',
    method: 'get',
    params
  })
}

export default { get }
