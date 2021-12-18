import request from '@/utils/request'

/**
 *
 * 获取生产看板-结构详情
 * @export
 * @param {*} id|required 产品id
 * @returns
 */
export function artifactDetail(params) {
  return request({
    module: 'mes',
    url: 'kanban/product/detail/artifact',
    method: 'get',
    params
  })
}

/**
 *
 * 获取生产看板-组立详情
 * @export
 * @param {*} id|required 产品id
 * @returns
 */
export function assembleDetail(params) {
  return request({
    module: 'mes',
    url: 'kanban/product/detail/assemble',
    method: 'get',
    params
  })
}
