import request from '@/utils/request'

/**
 *订单列表
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
    module: 'wms',
    url: 'logistics-order',
    method: 'get',
    params
  })
}

/**
 * 导出采购订单
 * @param {*} id  采购订单id
 */
export function download(id) {
  return request({
    module: 'wms',
    url: `purchase-order/export`,
    method: 'get',
    responseType: 'blob',
    params: {
      id
    }
  })
}

export default { get, download }
