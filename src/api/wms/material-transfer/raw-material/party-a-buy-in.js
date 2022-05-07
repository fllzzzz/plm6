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

/**
 * 甲供买入excel导出
 */
export function exportExcel(params) {
  return request({
    module: 'wms',
    url: `transfer/party-a-buy-in/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}

export default { get }
