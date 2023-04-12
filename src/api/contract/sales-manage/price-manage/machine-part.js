import request from '@/utils/request'

/**
 * 获取散发制品商务列表
 * @param {number} projectId | required 项目id
 * @param {number} monomerId | required 单体id
 */
export function get(params) {
  return request({
    module: 'contract',
    url: 'business/machine-part',
    method: 'get',
    params
  })
}

/**
 * 获取散发制品商务汇总
 * @param {number} monomerId | required 单体id
 */
export function cost({ monomerId }) {
  return request({
    module: 'contract',
    url: 'business/machine-part/summary',
    method: 'get',
    params: { monomerId }
  })
}

/**
 * 商务详情
 * @param {*} businessId 必填，商务构件id
 */
export function detail(businessId) {
  return request({
    module: 'contract',
    url: `business/machine-part/${businessId}`,
    method: 'get'
  })
}

export default { get, detail }
