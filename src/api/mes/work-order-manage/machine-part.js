import request from '@/utils/request'

/**
 * @description: 获取零件工单
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/order/machinePart/page',
    method: 'get',
    params
  })
}

/**
 * @description: 获取零件工单详情
*/
export function partDetail(params) {
  return {
    module: 'mes',
    url: 'task/order/machinePart/detail',
    method: 'get',
    params
  }
}

export default { get }
