import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'artifactMachinePart/listByCondition',
    method: 'get',
    params
  })
}


export function edit(data) {
  return request({
    module: 'plan',
    url: 'artifact/update',
    method: 'put',
    data
  })
}

export function editStatus(type,id) {
  return request({
    module: 'plan',
    url: `artifactMachinePart/updateStatus/${type}/${id}`,
    method: 'put'
  })
}

export function del(data) {
  return request({
    module: 'plan',
    url: 'artifactMachinePart/deleteArtifact',
    method: 'delete',
    data
  })
}

export function listUpload(data) {
  return request({
    module: 'plan',
    url: 'artifactMachinePart/import',
    responseType: 'blob',
    method: 'post',
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

/**
 * 下载零构件关联清单
 */
 export function downloadArtifactTree(params) {
  return request({
    module: 'plan',
    url: 'artifactMachinePart/export',
    responseType: 'blob',
    method: 'get',
    params
  })
}

/**
 * 下载零构件技术清单模板
 */
export function downloadArtifactTreeTemplate() {
  return request({
    module: 'plan',
    url: 'artifactMachinePart/template/export',
    responseType: 'blob',
    method: 'get'
  })
}

export default { edit, del, get }
