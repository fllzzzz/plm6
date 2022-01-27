import request from '@/utils/request'

/**
 * 车次跟踪
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {Array} projectIds 项目ids
 * @param {number} auditEndDate 发运结束时间
 * @param {number} auditStartDate 发运开始时间
 * @param {string} licensePlate 车牌
 * @param {string} manufactureType 制造类型
 * @param {string} serialNumber 车次编号
 * @returns
 */
export function get(params) {
  return request({
    module: 'cost',
    url: 'business/train/tracking',
    method: 'get',
    params
  })
}

/**
 * 车次跟踪详情
 * @param {number} id
 */
export function detail(id) {
  return request({
    module: 'cost',
    url: `business/train/tracking/${id}`,
    method: 'get'
  })
}

export default { get, detail }
