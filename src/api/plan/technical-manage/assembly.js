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

export function del(id) {
  return request({
    module: 'plan',
    url: `assemble​/deleteAssemble​/${id}`,
    method: 'delete'
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
 * 下载组立清单
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
 * 下载组立模板
 */
export function downloadAssembleTemplate() {
  return request({
    module: 'plan',
    url: 'assemble/download',
    responseType: 'blob',
    method: 'get'
  })
}

export default { edit, del, get }