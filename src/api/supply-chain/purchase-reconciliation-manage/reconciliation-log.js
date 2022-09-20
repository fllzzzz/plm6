import request from '@/utils/request'

/**
 * 供应商对账列表
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {array} createTime 时间
 * @param {string} serialNumber 订单号
 * @returns
 */
export function get(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/order-payment/reconciliation',
    method: 'get',
    params
  })
}

/**
 * 供应商对账excel导出
 */
export function excel(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/order-payment/reconciliation/export',
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}

export default { get, excel }
