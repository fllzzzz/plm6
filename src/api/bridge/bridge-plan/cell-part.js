import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'bridge',
    url: 'tech-element',
    method: 'get',
    params
  })
}

/**
 * 下载零构件关联清单
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
    module: 'bridge',
    url: 'tech-machinePart',
    method: 'get',
    params
  })
}

export default { get }
