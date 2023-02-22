import request from '@/utils/request'

/**
 * @description: 获取工序清单详情
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'report/product/team/summary/review',
    method: 'get',
    params
  })
}
/**
 * @description: 班组报表详情
 */
export function getProcessList(params) {
  return request({
    module: 'mes',
    url: 'report/product/team/summary/list',
    method: 'get',
    params
  })
}

export default { get }
