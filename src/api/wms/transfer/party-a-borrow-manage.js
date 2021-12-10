import request from '@/utils/request'

/**
 * 甲供材料借出列表
 *
 * @param {Array} createTime  创建时间
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'transfer/party-a/borrow',
    method: 'get',
    params
  })
}

export default {
  get
}
