import request from '@/utils/request'

/**
 * 获取辅材商务列表
 * @param {number} projectId | required 项目id
 * @param {number} monomerId | required 单体id
 */
export function get(params) {
  return request({
    module: 'contract',
    url: 'business/auxiliary-material',
    method: 'get',
    params
  })
}

/**
 * 获取辅材商务汇总
 * @param {number} projectId | required 项目id
 * @param {number} monomerId | required 单体id
 */
export function cost({ projectId, monomerId }) {
  return request({
    module: 'contract',
    url: 'business/auxiliary-material/summary',
    method: 'get',
    params: { projectId, monomerId }
  })
}

/**
 * 商务详情
 * @param {*} businessId 必填，商务构件id
 */
export function businessDetail(businessId) {
  return request({
    module: 'contract',
    url: `business/auxiliary-material/${businessId}`,
    method: 'get'
  })
}

export default { get }
