import request from '@/utils/request'

/**
 *
 * 获取生产看板-围护详情
 * @export
 * @param {*} id|required 产品id
 * @returns
 */
export function detail(params) {
  return request({
    module: 'mes',
    url: 'kanban/product/detail/enclosure',
    method: 'get',
    params
  })
}
