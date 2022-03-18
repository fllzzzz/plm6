import request from '@/utils/request'

/**
 * 物料标签-单据列表
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'material/label-print/receipt-mode',
    method: 'get',
    params
  })
}

/**
 * 物料标签-单据列表
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function detail(id) {
  return request({
    module: 'wms',
    url: `material/label-print/receipt-mode/${id}`,
    method: 'get'
  })
}

export default { get, detail }
