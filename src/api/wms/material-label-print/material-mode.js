import request from '@/utils/request'

/**
 * 物料列表
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'material/label-print/material-mode',
    method: 'get',
    params
  })
}

export default { get }
