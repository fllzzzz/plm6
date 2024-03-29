import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/logistics/summary',
    method: 'get',
    params
  })
}

// 物流记录
export function logisticsRecordDetail(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/logistics/record',
    method: 'get',
    params
  })
}

// 物流付款汇总
export function logisticsPaymentList(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/logistics-payment/detail',
    method: 'get',
    params
  })
}

// 获取开票明细
export function invoiceRecord(params) {
  return request({
    module: 'contract',
    url: 'contract/receive-invoice/logistics',
    method: 'get',
    params
  })
}
export default { get }
