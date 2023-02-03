import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `contract/payment/payment/ledger/invoice`,
    method: 'get',
    params
  })
}

export function invoiceSum(params) {
  return request({
    module: 'contract',
    url: `contract/payment/payment/ledger/invoice/summary`,
    method: 'get',
    params
  })
}
export default { get }
