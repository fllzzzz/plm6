import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/purchaseLedger',
    method: 'get',
    params
  })
}

export default { get }
