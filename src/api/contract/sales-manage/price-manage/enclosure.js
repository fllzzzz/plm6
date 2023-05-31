import request from '@/utils/request'

/**
 * 获取围护商务列表
 * @param {number} projectId | required 项目id
 * @param {number} monomerId | required 单体id
 */
export function get(params) {
  return request({
    url: '/api/business/enclosure',
    method: 'get',
    params
  })
}

/**
 * 获取围护商务汇总
 * @param {number} projectId | required 项目id
 * @param {number} monomerId | required 单体id
 */
export function cost({ projectId, monomerId }) {
  return request({
    url: '/api/business/enclosure/summary',
    method: 'get',
    params: { projectId, monomerId }
  })
}

export default { get }
