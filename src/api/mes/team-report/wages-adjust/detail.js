import request from '@/utils/request'

/**
 *
 * 获取工序价格
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'wages/product/page',
    method: 'get',
    params
  })
}

export default {
  get
}
