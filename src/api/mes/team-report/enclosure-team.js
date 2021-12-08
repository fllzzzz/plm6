import request from '@/utils/request'

/**
 *
 * 获取班组报表-围护
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'team/report/enclosure',
    method: 'get',
    params
  })
}

/**
 *
 * 获取班组报表-围护-详情
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'team/report/enclosure/detail',
    method: 'get',
    params
  })
}

export default {
  get
}
