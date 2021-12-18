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
    url: 'kanban/product',
    method: 'get',
    params
  })
}
