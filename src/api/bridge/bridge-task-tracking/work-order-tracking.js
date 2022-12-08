import request from '@/utils/request'

/**
 * @description: 获取构件工单跟踪列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/order/tracking/artifact/page',
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
    url: 'task/order/tracking/machine_part/page',
    method: 'get',
    params
  })
}
/**
 * @description: 获取构件部件工序进度（传统线）
 */
export function process(params) {
  return request({
    module: 'mes',
    url: 'task/process/artifact/process',
    method: 'get',
    params
  })
}
/**
 * @description: 获取构件部件工序进度（智能线 ）
 */
export function smartLineProcess(params) {
  return request({
    module: 'mes',
    url: 'task/process/artifact/page',
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
    url: 'task/process/machine_part/process',
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
    url: 'task/process/product/page',
    method: 'get',
    params
  })
}

export default { get }
