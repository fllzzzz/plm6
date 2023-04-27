import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'artifact-processFile',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'plan',
    url: `artifact-processFile`,
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'plan',
    url: `artifact-processFile`,
    method: 'put',
    data
  })
}

// 获取项目构件分类
export function getStructureClass(params) {
  return request({
    module: 'plan',
    url: 'artifact-processFile/artifactClass-snQuantity',
    method: 'get',
    params
  })
}

// 获取构件编号列表
export function getStructureList(params) {
  return request({
    module: 'plan',
    url: 'artifact-processFile/artifactSnList',
    method: 'get',
    params
  })
}

export function del(ids) {
  return request({
    module: 'plan',
    url: 'otherData',
    method: 'delete',
    data: ids
  })
}

export function download({ id }) {
  return request({
    module: 'plan',
    url: `otherData/${id}/export `,
    method: 'get',
    responseType: 'blob'
  })
}

export function downloadByProject(projectId, type) {
  return request({
    module: 'plan',
    url: `otherData/project/export`,
    method: 'get',
    timeout: 6000000,
    responseType: 'blob',
    params: { projectId, type }
  })
}

export function upload(data) {
  return request({
    module: 'plan',
    url: `otherData/upload`,
    method: 'post',
    timeout: 6000000,
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

export default { get, del, add, edit }
