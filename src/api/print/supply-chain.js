import request from '@/utils/request'

/**
 * 收票记录
 */
export function invoiceDetail(params) {
  return request({
    module: 'contract',
    url: `contract/receive-invoice/list/print`,
    method: 'get',
    params
  })
}

/**
 * 入库记录
 */
export function inboundRecord(params) {
  return request({
    module: 'contract',
    url: `supply-chain/order-payment/listInbound/print`,
    method: 'get',
    params
  })
}

/**
 * 付款记录
 */
export function paymentRecord(params) {
  return request({
    module: 'contract',
    url: `contract/payment/list/print`,
    method: 'get',
    params
  })
}

/**
 * 采购订单付款台账
 */
export function orderPaymentLedger(params) {
  return request({
    module: 'contract',
    url: `supply-chain/order-payment/print`,
    method: 'get',
    params
  })
}

/**
 * 供应商付款台账
 */
export function supplierPaymentLedger(params) {
  return request({
    module: 'contract',
    url: `supply-chain/order-payment/sumBySupplier/print`,
    method: 'get',
    params
  })
}

/**
 * 物流记录
 */
export function logisticsRecord(params) {
  return request({
    module: 'contract',
    url: `supply-chain/logistics/record/print`,
    method: 'get',
    params
  })
}

export default {
  invoiceDetail, // 收票记录
  paymentRecord, // 付款记录
  inboundRecord, // 入库记录
  orderPaymentLedger, // 采购订单付款台账
  supplierPaymentLedger, // 供应商付款台账
  logisticsRecord // 物流记录
}
