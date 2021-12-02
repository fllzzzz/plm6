import request from '@/utils/request'

/**
 *
 * 获取生产报表-围护
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'production_statements/enclosure/page',
    method: 'get',
    params
  })
}

/**
 *
 * 获取生产报表-围护
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function getSummary(params) {
  return request({
    module: 'mes',
    url: 'production_statements/enclosure/summary',
    method: 'get',
    params
  })
}

export default {
  get
}
