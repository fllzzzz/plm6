import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `contract/payment/payment/ledger`,
    method: 'get',
    params
  })
}

export function paymentSum(params) {
  return request({
    module: 'contract',
    url: `contract/payment/payment/ledger/summary`,
    method: 'get'
  })
}
export default { get }
