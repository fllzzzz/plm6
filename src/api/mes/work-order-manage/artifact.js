
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

/**
 * @description: 结构工单：打印标记
*/
export function printSign(params) {
  return request({
    module: 'mes',
    url: `task/process/task/list/sign`,
    method: 'get',
    params
  })
}

/**
 * @description: 获取结构工单项目信息
*/
export function getProjectInfo(params) {
  return request({
    module: 'mes',
    url: `task/order/artifact/project/list`,
    method: 'get',
    params
  })
}

/**
 * @description: 工单撤回
*/
export function backWorkOrder(ids) {
  return request({
    url: `api/mes/building/task/task/revoke`,
    method: 'delete',
    data: ids
  })
}

/**
 * @description: 查询整个详情工单是否可撤回
*/
export function getInitBack(params) {
  return request({
    url: `api/mes/building/task/process/task_order/revoke`,
    method: 'get',
    params
  })
}

/**
 * @description: 整个详情工单撤回
*/
export function allOrderBatch(id) {
  return request({
    url: `api/mes/building/task/task_order/revoke/${id}`,
    method: 'delete',
    data: id
  })
}

export default { get }

