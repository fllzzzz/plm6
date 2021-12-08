import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'otherData',
    method: 'get',
    params
  })
}

export function edit(id, params) {
  return request({
    module: 'plan',
    url: `otherData/${id}`,
    method: 'put',
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
    url: `otherData/${id}/export`,
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

export default { get, del, download }
