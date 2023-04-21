import request from '@/utils/request'

/**
 * 获取班组产量
 * @export
 * @param {*} name|required 页码
 * @param {*} size|required 页大小
 * @param {number} projectId 项目id
 * @param {number} startTime 开始时间
 * @param {number} endTime 结束时间
 * @returns
 * @returns
 */
export function get(params) {
  return request({
    module: 'enclosure',
    url: 'produce/overview/order/team/produce',
    method: 'get',
    params
  })
}

/**
 * 获取班组产量详情
 * @export
 * @param {*} name|required 页码
 * @param {*} size|required 页大小
 * @param {number} projectId|required 项目id
 * @param {number} lineId|required 产线id
 * @param {number} teamId|required 班组id
 * @param {number} price|required 价格
 * @param {number} startTime 开始时间
 * @param {number} endTime 结束时间
 * @returns
 */
export function detail(params) {
  return request({
    module: 'enclosure',
    url: 'produce/overview/order/team/produce/detail',
    method: 'get',
    params
  })
}

export default { get, detail }
