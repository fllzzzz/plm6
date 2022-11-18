import request from '@/utils/request'

/**
 * 验收记录列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {array} inboundTime 时间
 * @param {string} year 年份
 * @param {number} supplierId 供应商id
 * @param {number} purchaseOrderId 采购合同id
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
