
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
    url: 'task/process/list',
    method: 'get',
    params
  })
}

/**
 * @description: 获取任务清单
*/
export function getTaskList(params) {
  return request({
    module: 'mes',
    url: 'task/process/product/task/list/page',
    method: 'get',
    params
  })
}

/**
 * @description: 获取部件套料清单
*/
export function getNestingList(params) {
  return request({
    module: 'mes',
    url: 'task/process/nesting/task/list/page',
    method: 'get',
    params
  })
}

export default { get }

