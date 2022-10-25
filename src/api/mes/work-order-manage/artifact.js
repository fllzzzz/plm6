
import request from '@/utils/request'

/**
 * @description: 任务工单列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/order/artifact/page',
    method: 'get',
    params
  })
}

/**
 * @description: 获取生产任务单
 */
export function productTask(params) {
  return request({
    module: 'mes',
    url: 'task/order/product/task',
    method: 'get',
    params
  })
}

/**
 * @description: 获取用户的工序列表
*/
export function processInfo(params) {
  return request({
    module: 'mes',
    url: 'task/order/process/list',
    method: 'get',
    params
  })
}

export default { get }

