import request from '@/utils/request'

/**
 *
 * 获取生产统计汇总
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'analysis/hysteresis',
    method: 'get',
    params
  })
}

/**
 *
 * 获取生产统计详情
 */
export function getDetail(params) {
  return request({
    module: 'mes',
    url: 'analysis/hysteresis/details',
    method: 'get',
    params
  })
}

export default {
  get
}
