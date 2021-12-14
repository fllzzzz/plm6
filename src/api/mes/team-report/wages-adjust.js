import request from '@/utils/request'

/**
 *
 * 获取工序价格
 */
export function get({ monomerId, productType, projectId }) {
  return request({
    module: 'mes',
    url: 'wages',
    method: 'get',
    params: { monomerId, productType, projectId }
  })
}

/**
 *
 * 修改工序价格
 */
export function detail(data) {
  return request({
    module: 'mes',
    url: 'wages',
    method: 'put',
    data
  })
}

/**
 *
 * 审核工序价格
 */
export function check(data) {
  return request({
    module: 'mes',
    url: 'wages/status',
    method: 'put',
    data
  })
}

export default {
  get
}
