import request from '@/utils/request'

/**
 *
 * 获取生产看板
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function productDashboard(params) {
  return request({
    module: 'mes',
    url: 'kanban/product/page',
    method: 'get',
    params
  })
}

/**
 *
 * 获取生产规格
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function productSpec(params) {
  return request({
    module: 'mes',
    url: 'kanban/product/steelSpec',
    method: 'get',
    params
  })
}
