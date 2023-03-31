import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/scm/subLedger',
    method: 'get',
    params
  })
}

// 付款汇总
export function paymentRecord(params) {
  return request({
    url: '/api/scm/subLedger/listPayment',
    method: 'get',
    params
  })
}

// 获取开票明细
export function invoiceRecord(params) {
  return request({
    url: '/api/scm/subLedger/listInvoice',
    method: 'get',
    params
  })
}
export default { get }
