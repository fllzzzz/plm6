import request from '@/utils/request'

/**
 *
 * 获取编外计价制
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'wages/in_staff/distribution',
    method: 'get',
    params
  })
}

/**
 *
 * 获取获取编外计价制详情
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'wages/in_staff/distribution/complete',
    method: 'get',
    params
  })
}

export default {
  get
}
