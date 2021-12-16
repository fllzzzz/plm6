import request from '@/utils/request'

/**
 * 甲供买入记录
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'transfer/party-a-buy-in',
    method: 'get',
    params
  })
}

export default { get }
