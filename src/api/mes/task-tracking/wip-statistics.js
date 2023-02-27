import request from '@/utils/request'

/**
 * @description: 获取在制品统计列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/process/project/progressing/page',
    method: 'get',
    params
  })
}
/**
 * @description: 获取钢材出库记录
 */
export function getOutbound(params) {
  return request({
    module: 'mes',
    url: 'task/process/project/progressing/outBound/page',
    method: 'get',
    params
  })
}
/**
 * @description: 获取排产记录
 */
export function getTask(params) {
  return request({
    module: 'mes',
    url: 'task/process/project/progressing/task/page',
    method: 'get',
    params
  })
}
/**
 * @description: 获取工序在制品记录
 */
export function getProcess(params) {
  return request({
    module: 'mes',
    url: 'task/process/project/progressing/process/page',
    method: 'get',
    params
  })
}

export default { get }
