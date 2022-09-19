import request from '@/utils/request'

/**
 * 验收记录列表
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
    url: 'supply-chain/order-payment/acceptance',
    method: 'get',
    params
  })
}

/**
 * 验收记录excel导出
 */
export function excel(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/order-payment/acceptance/export',
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}

export default { get, excel }
