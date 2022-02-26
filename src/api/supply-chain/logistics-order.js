import request from '@/utils/request'

/**
 * 物流订单列表（入库审核通过的）
 *
 * @param {String} serialNumber 订单号/合同号
 * @param {Array} createTime  创建时间
 * @param {String} supplierName 供应商名称
 * @param {Number} type 订单类型
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'scm',
    url: 'logistics-order',
    method: 'get',
    params
  })
}

export default { get }
