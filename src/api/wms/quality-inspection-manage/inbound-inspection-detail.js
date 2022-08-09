import request from '@/utils/request'

/**
 * 原材料入库申请列表
 *
 * @param {String} serialNumber 订单号/合同号
 * @param {Array} createTime  创建时间
 * @param {String} supplierName 供应商名称
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'inbound/application/record/quality-testing/detail',
    method: 'get',
    params
  })
}

export default { get }
