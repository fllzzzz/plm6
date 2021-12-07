import request from '@/utils/request'

/**
 *
 * 获取qhse列表
 * @export
 * @param {*} page|required 页码
 * @param {*} size|required 页大小
 * @returns
 */
export function get(params) {
  return request({
    module: 'mes',
    url: 'qhse/page',
    method: 'get',
    params
  })
}

export default {
  get
}
