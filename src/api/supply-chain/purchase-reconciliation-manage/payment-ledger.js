import request from '@/utils/request'

/**
 * 客户交易列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} endDate 结束时间
 * @param {number} startDate 开始时间
 * @param {number} propertyType 属性 1原材料采购 2制成品采购 4原材料运输 8制成品运输 16专业分包
 * @param {string} supplierName 供应商
 * @param {string} serialNumber 订单号
 * @returns
 */
export function get(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/order-payment',
    method: 'get',
    params
  })
}

// 按订单汇总
export function getBySupplier(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/order-payment/sumBySupplier',
    method: 'get',
    params
  })
}
/**
 * 付款记录
 * @param {number} orderId|required 订单id
 * @param {number} propertyType 属性 1原材料采购 2制成品采购 4原材料运输 8制成品运输 16专业分包
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 */
export function paymentRecord(params) {
  return request({
    module: 'contract',
    url: `supply-chain/order-payment/listPayment/${params.orderId}`,
    method: 'get',
    params,
    cancelKey: false
  })
}

/**
 * 开票记录
 * @param {number} orderId|required 订单id
 * @param {number} propertyType 属性 1原材料采购 2制成品采购 4原材料运输 8制成品运输 16专业分包
 */
export function invoiceRecord(params) {
  return request({
    module: 'contract',
    url: `supply-chain/order-payment/listInvoice/${params.orderId}`,
    method: 'get',
    params
  })
}

/**
 * 入库记录
 * @param {number} orderId|required 订单id
 * @param {number} propertyType 属性 1原材料采购 2制成品采购 4原材料运输 8制成品运输 16专业分包
 */
export function inboundRecord(params) {
  return request({
    module: 'contract',
    url: `supply-chain/order-payment/listInbound/${params.orderId}`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export default { get }
