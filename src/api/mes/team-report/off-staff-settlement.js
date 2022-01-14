import request from '@/utils/request'

/**
 *
 * 编外结算列表
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'wages/out_staff/summary',
    method: 'get',
    params
  })
}

/**
 *
 * 编外结算列表详情
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'wages/out_staff/details',
    method: 'get',
    params
  })
}

export default {
  get
}
