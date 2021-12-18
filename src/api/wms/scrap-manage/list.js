import request from '@/utils/request'

/**
 * 钢材废料列表
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function getSteelList(params) {
  return request({
    module: 'wms',
    url: 'scrap/steel',
    method: 'get',
    params
  })
}
