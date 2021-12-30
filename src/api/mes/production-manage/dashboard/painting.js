import request from '@/utils/request'

/**
 *
 * 获取涂装列表
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'kanban/painting/list',
    method: 'get',
    params
  })
}

/**
 *
 * 涂装修改
 */
export function change(data) {
  return request({
    module: 'mes',
    url: 'kanban/painting/change',
    method: 'put',
    data
  })
}

/**
 *
 * 涂装面积修改
 */
export function areaChange(data) {
  return request({
    module: 'mes',
    url: 'kanban/painting/area/change',
    method: 'put',
    data
  })
}

export default { get }
