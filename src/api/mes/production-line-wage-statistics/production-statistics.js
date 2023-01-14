import request from '@/utils/request'

/**
 * @description: 根据条件获取工序价格汇总
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'price/process/list',
    method: 'get',
    params
  })
}

/**
 * @description: 根据工序获取对应的详情
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'price/process/details/list',
    method: 'get',
    params
  })
}

/**
 * @description: 根据时间获取全年价格汇总
 */
export function getSummary(params) {
  return request({
    module: 'mes',
    url: 'price/summary',
    method: 'get',
    params
  })
}

/**
 * @description: 导出清单
 */
export function exportListFn(params) {
  return request({
    module: 'mes',
    url: 'price/export',
    method: 'get',
    responseType: 'blob',
    params
  })
}

export default { get }
