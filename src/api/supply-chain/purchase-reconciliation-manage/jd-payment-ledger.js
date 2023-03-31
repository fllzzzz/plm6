import request from '@/utils/request'

// 按供应商汇总
export function get(params) {
  return request({
    url: '/api/scm/purchaseLedger',
    method: 'get',
    params
  })
}
/**
 * 付款记录（下面两个字段必传一个）
 * @param {number} orderId|required 订单id
 * @param {number} supplierId|required 采购供应商id
 * @param {number} propertyType|required 属性 1采购合同 2物流费
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 */
export function paymentRecord(params) {
  return request({
    url: `/api/scm/purchaseLedger/listPayment`,
    method: 'get',
    params,
    cancelKey: false
  })
}

/**
 * 收票记录（下面两个字段必传一个）
 * @param {number} orderId|required 订单id
 * @param {number} supplierId|required 采购供应商id
 */
export function invoiceRecord(params) {
  return request({
    url: `/api/scm/purchaseLedger/listInvoice`,
    method: 'get',
    params,
    cancelKey: false
  })
}

/**
 * 入库记录（下面两个字段必传一个）
 * @param {number} orderId|required 订单id
 * @param {number} supplierId|required 采购供应商id
 */
export function inboundRecord(params) {
  return request({
    module: 'contract',
    url: `supply-chain/order-payment/listInbound`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export default { get }
