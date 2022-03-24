import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/payment/order',
    method: 'get',
    params
  })
}

export default { get }
