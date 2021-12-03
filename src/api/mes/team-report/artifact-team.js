import request from '@/utils/request'

/**
 *
 * 获取班组报表-结构
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'team/report/artifact',
    method: 'get',
    params
  })
}

/**
 *
 * 获取班组报表-结构-详情
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'team/report/artifact/detail',
    method: 'get',
    params
  })
}

/**
 *
 * 获取班组报表-结构-工序详情
 */
export function processDetail(params) {
  return request({
    module: 'mes',
    url: 'team/report/artifact/detail/process',
    method: 'get',
    params
  })
}

export default {
  get
}
