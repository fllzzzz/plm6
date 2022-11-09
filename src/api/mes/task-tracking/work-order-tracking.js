import request from '@/utils/request'

/**
 * @description: 获取构件工单跟踪列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/tracking/artifact/page',
    method: 'get',
    params
  })
}
/**
 * @description: 获取零件工单跟踪列表
 */
export function machinePart(params) {
  return request({
    module: 'mes',
    url: 'task/tracking/machine_part/page',
    method: 'get',
    params
  })
}
/**
 * @description: 获取构件部件工序进度
 */
export function process(params) {
  return request({
    module: 'mes',
    url: 'task/artifact/process',
    method: 'get',
    params
  })
}
/**
 * @description: 获取零件工序进度
 */
export function machineProcess(params) {
  return request({
    module: 'mes',
    url: 'task/machine_part/process',
    method: 'get',
    params
  })
}
/**
 * @description: 工序进度详情
 */
export function processDetail(params) {
  return request({
    module: 'mes',
    url: 'task/process/product',
    method: 'get',
    params
  })
}

export default { get }