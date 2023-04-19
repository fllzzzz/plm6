import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/scm/cargoListLedger',
    method: 'get',
    params
  })
}

// 物流记录
export function logisticsRecordDetail(params) {
  return request({
    url: '/api/scm/cargoListLedger/listCargoList',
    method: 'get',
    params
  })
}

// 物流付款汇总
export function paymentRecord(params) {
  return request({
    url: '/api/scm/cargoListLedger/listPayment',
    method: 'get',
    params
  })
}

// 获取开票明细
export function invoiceRecord(params) {
  return request({
    url: '/api/scm/cargoListLedger/listInvoice',
    method: 'get',
    params
  })
}
export default { get }
