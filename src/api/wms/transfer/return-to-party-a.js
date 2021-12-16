import request from '@/utils/request'

/**
 * 归还甲方记录
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'transfer/return-to-party-a',
    method: 'get',
    params
  })
}

export default { get }
