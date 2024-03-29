import request from '@/utils/request'

/**
 *
 * 获取生产看板-分段详情
 * @export
 * @param {*} id|required 产品id
 * @returns
 */
export function boxDetail(params) {
  return request({
    module: 'bridge',
    url: 'kanban/product/detail/box',
    method: 'get',
    params
  })
}

/**
 *
 * 获取生产看板-单元件详情
 * @export
 * @param {*} id|required 产品id
 * @returns
 */
export function elementDetail(params) {
  return request({
    module: 'bridge',
    url: 'kanban/product/detail/element',
    method: 'get',
    params
  })
}

// 获取生产看板-母件下的单元件详情
export function baseElementDetail(id) {
  return request({
    module: 'bridge',
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
    module: 'bridge',
    url: 'kanban/product/detail/machine_part',
    method: 'get',
    params
  })
}
