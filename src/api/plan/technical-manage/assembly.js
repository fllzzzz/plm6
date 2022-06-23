import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'assemble/listAssemble',
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'plan',
    url: 'assemble/updateAssemble',
    method: 'put',
    data
  })
}

// export function del(id) {
//   return request({
//     module: 'plan',
//     url: 'assemble/deleteAssemble/' + id,
//     method: 'delete'
//   })
// }
export function del(data) {
  return request({
    module: 'plan',
    url: 'assemble/deleteAssembleList',
    method: 'delete',
    data
  })
}

export function delAssemblyArtifact(params) {
  return request({
    module: 'plan',
    url: `assemble/deleteArtifact`,
    method: 'delete',
    params
  })
}

// 按区域一键清空
export function delAssemblyByArea(params) {
  return request({
    module: 'plan',
    url: 'assemble/clear',
    method: 'delete',
    params
  })
}

export function addAssemblyArtifact(data) {
  return request({
    module: 'plan',
    url: 'assemble/saveArtifact',
    method: 'post',
    data
  })
}

export function listUpload(data) {
  return request({
    module: 'plan',
    url: 'assemble/import',
    responseType: 'blob',
    method: 'post',
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

/**
 * 下载部件清单
 */
export function downloadAssemble(params) {
  return request({
    module: 'plan',
    url: 'assemble/export',
    responseType: 'blob',
    method: 'get',
    params
  })
}

/**
 * 下载部件模板
 */
export function downloadAssembleTemplate() {
  return request({
    module: 'plan',
    url: 'assemble/download',
    responseType: 'blob',
    method: 'get'
  })
}

// 获取异常编号
export function assembleError(params) {
  return request({
    module: 'plan',
    url: 'assemble/listAbnormal',
    method: 'get',
    params
  })
}

export default { edit, del, get }
