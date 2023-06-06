import request from '@/utils/request'

/**
 * @description: 获取工序呆滞列表详情
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'task/process/dull/process/detail/page',
    method: 'get',
    params
  })
}
/**
 * @description: 获取当前车间下的生产线
 */
export function productionLineProcess(params) {
  return request({
    module: 'bridge',
    url: 'task/workShop/productionLine',
    method: 'get',
    params
  })
}
/**
 * @description: 获取工序呆滞列表
 */
export function processSluggish(params) {
  return request({
    module: 'bridge',
    url: 'task/process/dull/process',
    method: 'get',
    params
  })
}
/**
 * @description: 获取车间下的所有工序
 */
export function getAllProcess(params) {
  return request({
    module: 'bridge',
    url: 'task/process/dull/process',
    method: 'get',
    params
  })
}

export default { get }

