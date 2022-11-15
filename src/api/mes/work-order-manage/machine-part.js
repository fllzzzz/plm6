import request from '@/utils/request'

/**
 * @description: 获取零件工单
 */
export function get(params) {
  return request({
    module: 'mes',
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
    module: 'mes',
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
    module: 'mes',
    url: 'task/order/nesting',
    method: 'get',
    responseType: 'blob',
    params
  })
}
/**
 * @description: 获取钻孔任务单
*/
export function showDrillDetail(params) {
  return request({
    module: 'mes',
    url: 'task/order/drilling',
    method: 'get',
    params
  })
}
/**
 * @description: 根据任务单号查询零件分拣信息(分页)
*/
export function showInfo(params) {
  return request({
    module: 'mes',
    url: `machine_part/separate/page`,
    method: 'get',
    params
  })
}
/**
 * @description: 打印分拣单
*/
export function printInfo(params) {
  return request({
    module: 'mes',
    url: `machine_part/separate`,
    method: 'get',
    params
  })
}

export default { get }
