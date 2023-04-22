import request from '@/utils/request'

/**
 * 收票记录
 */
export function invoiceDetail(params) {
  return request({
    url: `/api/scm/purchaseLedger/listInvoice/print`,
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
    url: `/api/scm/purchaseLedger/listPayment/print`,
    method: 'get',
    params
  })
}

/**
 * 采购合同付款台账
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
 * 原材料采购台账
 */
export function supplierPaymentLedger(params) {
  return request({
    url: `/api/scm/purchaseLedger/print`,
    method: 'get',
    params
  })
}

/**
 * 原材料物流记录
 */
export function logisticsRecord(params) {
  return request({
    module: 'contract',
    url: `supply-chain/logistics/record/print`,
    method: 'get',
    params
  })
}

/**
 * 制成品物流记录
 */
export function productLogisticsRecord(params) {
  return request({
    url: `/api/scm/cargoListLedger/listCargoList/print`,
    method: 'get',
    params
  })
}

/**
 * 制成品物流收票记录
 */
export function productLogisticsInvoiceRecord(params) {
  return request({
    url: `/api/scm/cargoListLedger/listInvoice/print`,
    method: 'get',
    params
  })
}

/**
 * 制成品物流付款记录
 */
export function productLogisticsPaymentRecord(params) {
  return request({
    url: `/api/scm/cargoListLedger/listPayment/print`,
    method: 'get',
    params
  })
}

/**
 * 原材料物流收票记录
 */
export function logisticsInvoiceRecord(params) {
  return request({
    url: `/api/scm/logisticsLedger/listInvoice/print`,
    method: 'get',
    params
  })
}

/**
 * 原材料物流付款记录
 */
export function logisticsPaymentRecord(params) {
  return request({
    url: `/api/scm/logisticsLedger/listPayment/print`,
    method: 'get',
    params
  })
}

/**
 * 分包订单收票记录
 */
export function subcontractInvoiceRecord(params) {
  return request({
    url: `/api/contract/subLedger/listInvoice/print`,
    method: 'get',
    params
  })
}

/**
 * 分包订单付款记录
 */
export function subcontractPaymentRecord(params) {
  return request({
    url: `/api/contract/subLedger/listPayment/print`,
    method: 'get',
    params
  })
}
/**
 * 申购详情
 */
export function requisitionsDetail({ id }) {
  return request({
    module: 'scm',
    url: `apply-purchase/${id}/print`,
    method: 'get'
  })
}

export default {
  invoiceDetail, // 收票记录
  paymentRecord, // 付款记录
  inboundRecord, // 入库记录
  orderPaymentLedger, // 采购合同付款台账
  supplierPaymentLedger, // 原材料采购台账
  logisticsRecord, // 原材料物流记录
  productLogisticsRecord, // 制成品物流记录
  productLogisticsInvoiceRecord, // 制成品物流收票记录
  productLogisticsPaymentRecord, // 制成品物流付款记录
  logisticsInvoiceRecord, // 原材料物流收票记录
  logisticsPaymentRecord, // 原材料物流付款记录
  subcontractInvoiceRecord,
  subcontractPaymentRecord,
  requisitionsDetail // 申购详情
}
