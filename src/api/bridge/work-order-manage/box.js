
import request from '@/utils/request'

/**
 * @description: 任务工单列表
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'task/order/box/page',
    method: 'get',
    params
  })
}

/**
 * @description: 获取生产任务单
 */
export function productTask(params) {
  return request({
    module: 'bridge',
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
    module: 'bridge',
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
    module: 'bridge',
    url: 'task/process/product/task/list/page',
    method: 'get',
    params
  })
}

/**
 * @description: 获取单元件套料清单
*/
export function getNestingList(params) {
  return request({
    module: 'bridge',
    url: 'task/process/nesting/task/list/page',
    method: 'get',
    params
  })
}

/**
 * @description: 结构工单：打印标记
*/
export function printSign(params) {
  return request({
    module: 'bridge',
    url: `task/process/task/list/sign`,
    method: 'get',
    params
  })
}

export default { get }

