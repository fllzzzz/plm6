import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/bridge/tech-element',
    method: 'get',
    params
  })
}

/**
 * 下载零分段关联清单
 */
// export function downloadArtifactTree(params) {
//   return request({
//     module: 'plan',
//     url: 'artifactMachinePart/export',
//     responseType: 'blob',
//     method: 'get',
//     params
//   })
// }

// 获取分段对应的单元
export function cellPartDetail(params) {
  return request({
    url: '/api/bridge/tech-machinePart',
    method: 'get',
    params
  })
}

export default { get }
