import request from '@/utils/request'

/**
 *
 * 获取生产线报表列表
 * @export
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'report/inspection/summary/passrate',
    method: 'get',
    params
  })
}

/**
 *
 * 获取生产线报表列表详情
 * @export
 * @returns
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'report/inspection/summary/review',
    method: 'get',
    params
  })
}

export default {
  get
}
