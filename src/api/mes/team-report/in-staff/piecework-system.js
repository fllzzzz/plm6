import request from '@/utils/request'

/**
 *
 * 获取编内计价制
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'wages/in_staff/reckon',
    method: 'get',
    params
  })
}

/**
 *
 * 获取获取编内计价制详情
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'wages/in_staff/reckon/details',
    method: 'get',
    params
  })
}

export default {
  get
}