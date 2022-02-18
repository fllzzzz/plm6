import request from '@/utils/request'

/**
 *
 * 获取零件齐套列表
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'kanban/assemble_matching/page',
    cancelKey: false,
    method: 'get',
    params
  })
}

/**
 *
 * 获取匹配零件
 * @export
 * @param {*} id|required 数组
 * @returns
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'kanban/assemble_matching/detail',
    method: 'get',
    params
  })
}

export default { get }
