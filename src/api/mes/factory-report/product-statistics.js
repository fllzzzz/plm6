import request from '@/utils/request'

/**
 * @description: 生产统计
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'task/process/produce/statistics',
    method: 'get',
    params
  })
}

/**
 * @description: 生产统计/原材料累计出库详情
 */
export function getTotalOutBound(params) {
  return request({
    module: 'mes',
    url: 'task/process/produce/statistics/outBound/page',
    method: 'get',
    params
  })
}

/**
 * @description: 生产统计/任务量详情
 */
export function getTask(params) {
  return request({
    module: 'mes',
    url: 'task/process/produce/statistics/task/page',
    method: 'get',
    params
  })
}

/**
 * @description: 生产统计/工序详情
 */
export function getProcess(params) {
  return request({
    module: 'mes',
    url: 'task/process/produce/statistics/process/page',
    method: 'get',
    params
  })
}

/**
 * @description: 生产统计/制成品详情
 */
export function getUps(params) {
  return request({
    module: 'mes',
    url: 'task/process/produce/statistics/finish/page',
    method: 'get',
    params
  })
}

export default { get }
