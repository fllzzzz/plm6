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
 * 获取生产看板-部件详情
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

// 获取生产看板-母件下的部件详情
export function baseAssembleDetail(id) {
  return request({
    module: 'mes',
    url: `section_steel/nesting/${id}/link`,
    method: 'get'
  })
}

/**
 *
 * 获取生产看板-零件详情
 * @export
 * @param {*} id|required 产品id
 * @returns
 */
export function machinePartDetail(params) {
  return request({
    module: 'mes',
    url: 'kanban/product/detail/machine_part',
    method: 'get',
    params
  })
}
