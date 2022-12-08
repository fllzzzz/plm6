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
    module: 'bridge',
    url: 'kanban/element_matching/page',
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
    module: 'bridge',
    url: 'kanban/element_matching/detail',
    method: 'get',
    params
  })
}

/**
 * @description：获取区域下零单元件生产数据
 */
export function productionDetail(params) {
  return request({
    module: 'bridge',
    url: 'kanban/element_matching/area/product',
    method: 'get',
    params
  })
}
/**
 * @description：获取区域下分段汇总
 */
export function artifactInfo(params) {
  return request({
    module: 'bridge',
    url: 'kanban/element_matching/area/box',
    method: 'get',
    params
  })
}

export default { get }
