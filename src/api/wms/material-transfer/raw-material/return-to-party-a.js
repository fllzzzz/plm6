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

/**
 * 归还甲方记录excel导出
 */
export function exportExcel(params) {
  return request({
    module: 'wms',
    url: `transfer/return-to-party-a/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}

export default { get }
