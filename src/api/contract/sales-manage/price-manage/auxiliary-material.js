import request from '@/utils/request'

/**
 * 获取辅材商务列表
 * @param {number} projectId | required 项目id
 * @param {number} monomerId | required 单体id
 */
export function get(params) {
  return request({
    module: 'contract',
    url: 'business/standardPart',
    method: 'get',
    params
  })
}

/**
 * 获取辅材商务汇总
 * @param {number} projectId | required 项目id
 * @param {number} monomerId | required 单体id
 */
// export function cost({ projectId, monomerId }) {
//   return request({
//     module: 'contract',
//     url: 'business/auxiliary-material/summary',
//     method: 'get',
//     params: { projectId, monomerId }
//   })
// }

// 保存标准件价格
export function saveStandardPart(data) {
  return request({
    module: 'contract',
    url: 'business/standardPart',
    method: 'put',
    data
  })
}
export default { get }
