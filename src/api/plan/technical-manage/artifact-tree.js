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

export function editStatus(type, id, params) {
  return request({
    module: 'plan',
    url: `artifactMachinePart/updateStatus/${type}/${id}`,
    method: 'put',
    params
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

export function changeListUpload(data) {
  return request({
    module: 'mes',
    url: 'tech/change/analyzing',
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

// 查询异常构件编号
export function errorArtifact(params) {
  return request({
    module: 'plan',
    url: 'artifactMachinePart/listAbnormal',
    method: 'get',
    params
  })
}

// 获取构件对应的零件
export function artifactPart(params) {
  return request({
    module: 'plan',
    url: 'artifactMachinePart/listMachinePart',
    method: 'get',
    params
  })
}

// 按区域一键清空
export function delArtifactTreeByArea(params) {
  return request({
    module: 'plan',
    url: 'artifactMachinePart/clear',
    method: 'delete',
    params
  })
}

// 数量变更
export function numChange(data) {
  return request({
    module: 'plan',
    url: 'artifact/change/quantity',
    method: 'put',
    data
  })
}

// 获取构件详情
export function artifactInfo(params) {
  return request({
    module: 'plan',
    url: 'artifact/info',
    method: 'get',
    params
  })
}

// 清单变更
export function listChange(data) {
  return request({
    module: 'plan',
    url: 'artifact/change/info',
    method: 'put',
    data
  })
}

// 清单变更
export function serialChange(data) {
  return request({
    module: 'plan',
    url: 'artifact/change/serialNumber',
    method: 'put',
    data
  })
}
export default { edit, del, get }
