import request from '@/utils/request'

/**
 * 可还库列表
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: `return/returnable/${params.basicClass}`,
    method: 'get',
    params
  })
}

export default { get }
