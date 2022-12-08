import request from '@/utils/request'

/**
 * @description: 获取产线跟踪列表
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'task/tracking/productionLine',
    method: 'get',
    params
  })
}
/**
 * @description: 获取产线跟踪详情
 */
export function productionLineDetail(params) {
  return request({
    module: 'bridge',
    url: 'task/tracking/productionLine/detail/page',
    method: 'get',
    params
  })
}

export default { get }
