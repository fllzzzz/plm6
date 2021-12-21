import request from '@/utils/request'

/**
 *
 * 获取班组报表-围护
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'team_form/enclosure',
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
    url: 'team_form/enclosure/details',
    method: 'get',
    params
  })
}

export default {
  get
}
