import request from '@/utils/request'

/**
 * 围护产品类型分析
 * @export
 * @param {number} projectId 项目id
 * @param {number} startTime 开始时间
 * @param {number} endTime 结束时间
 * @returns
 * @returns
 */
export function get(params) {
  return request({
    module: 'enclosure',
    url: 'produce/overview/order/category/produce/analysis',
    method: 'get',
    params
  })
}

/**
 * 围护类型规格分析
 * @export
 * @param {number} category|required 围护类型
 * @param {number} projectId|required 项目id
 * @param {number} startTime 开始时间
 * @param {number} endTime 结束时间
 * @returns
 */
export function detail(params) {
  return request({
    module: 'enclosure',
    url: 'produce/overview/order/plate/produce/analysis',
    method: 'get',
    params
  })
}

export default { get, detail }
