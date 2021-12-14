import request from '@/utils/request'

/**
 *
 * 入发存报表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'warehouse/report',
    method: 'get',
    params
  })
}

export function getSummary(params) {
  return request({
    module: 'mes',
    url: 'warehouse/report/summary',
    method: 'get',
    params
  })
}

export default { get }
