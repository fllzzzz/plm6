import request from '@/utils/request'

/**
 * 商务跟踪
 * @param {number} page|required 页码
 * @param {number} size|required 页大小
 * @param {number} projectId 项目id
 * @param {number} monomerId 单体id
 * @param {number} auditEndDate 发运结束时间
 * @param {number} auditStartDate 发运开始时间
 * @param {number} productType 产品类型
 * @param {string} name 名称
 * @param {string} licensePlate 车牌
 * @param {string} material 材质
 * @param {string} manufactureType 制造类型
 * @param {string} serialNumber 编号
 * @returns
 */
export function get(params) {
  return request({
    module: 'cost',
    url: 'business/tracking',
    method: 'get',
    params
  })
}

// 商务汇总
export function summary(params) {
  return request({
    module: 'cost',
    url: 'business/summary',
    method: 'get',
    params
  })
}

export default { get }
