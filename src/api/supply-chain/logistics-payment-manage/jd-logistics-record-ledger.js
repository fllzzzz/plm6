import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/scm/logisticsLedger',
    method: 'get',
    params
  })
}

// 物流记录
export function logisticsRecordDetail(params) {
  return request({
    url: '/api/contract/logisticsLedger/listLogistics',
    method: 'get',
    params
  })
}

// 物流付款汇总
export function paymentRecord(params) {
  return request({
    url: '/api/scm/logisticsLedger/listPayment',
    method: 'get',
    params
  })
}

// 获取开票明细
export function invoiceRecord(params) {
  return request({
    url: '/api/scm/logisticsLedger/listInvoice',
    method: 'get',
    params
  })
}

// 物流费变更
export function feeChange(data) {
  return request({
    url: '/api/scm/logistics-order/changeFee',
    method: 'post',
    data
  })
}

// 物流费变更记录
export function feeChangeRecord(params) {
  return request({
    url: '/api/scm/logistics-fee-record',
    method: 'get',
    params
  })
}

// 物流费变更审核
export function feeChangeAudit(data) {
  return request({
    url: '/api/scm/logistics-fee-record/check',
    method: 'put',
    data: [data]
  })
}

// 物流费待审核记录
export function feeUnAudit(params) {
  return request({
    url: '/api/scm/logistics-fee-record/unCheckCount',
    method: 'get',
    params
  })
}

export default { get }
