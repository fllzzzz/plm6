import request from '@/utils/request'

/**
 * @description: 获取零件工单
 */
export function get(params) {
  return request({
    module: 'bridge',
    url: 'task/order/machinePart/page',
    method: 'get',
    params
  })
}

/**
 * @description: 获取零件工单详情
*/
export function partDetail(params) {
  return request({
    module: 'bridge',
    url: 'task/order/machinePart/detail',
    method: 'get',
    params
  })
}
/**
 * @description: 获取套料任务单
*/
export function showCuttingPdf(params) {
  return request({
    module: 'bridge',
    url: 'task/order/nesting',
    method: 'get',
    responseType: 'blob',
    params
  })
}
/**
 * @description: 获取钻孔任务单
*/
export function productionTaskDetail(params) {
  return request({
    module: 'bridge',
    url: 'task/order/drilling',
    method: 'get',
    params
  })
}

/**
 * @description: 获取零件-分拣单
*/
export function getSeparateOrder(params) {
  return request({
    module: 'bridge',
    url: `machine_part/separate`,
    method: 'get',
    params
  })
}

/**
 * @description: 零件工单：打印标记
*/
export function printSign(params) {
  return request({
    module: 'bridge',
    url: `machine_part/separate/print/sign`,
    method: 'get',
    params
  })
}

export default { get }
