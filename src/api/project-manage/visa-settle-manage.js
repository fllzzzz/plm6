import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: 'foreign/visa-settle',
    method: 'get',
    params
  })
}

export function addVisa(data) {
  return request({
    module: 'project',
    url: 'foreign/visa-settle/visa',
    method: 'post',
    data
  })
}

export function editVisa(data) {
  return request({
    module: 'project',
    url: 'foreign/visa-settle/visa',
    method: 'put',
    data
  })
}

export function getProjectInfo(projectId) {
  return request({
    module: 'contract',
    url: `visa/project/${projectId}`,
    method: 'get'
  })
}

export function check(data) {
  return request({
    module: 'project',
    url: 'foreign/visa-settle/check',
    method: 'put',
    data
  })
}

export function addSettlement(data) {
  return request({
    module: 'project',
    url: 'foreign/visa-settle/settle',
    method: 'post',
    data
  })
}

export function editSettlement(data) {
  return request({
    module: 'project',
    url: 'foreign/visa-settle/settle',
    method: 'put',
    data
  })
}

export function detail(id) {
  return request({
    module: 'contract',
    url: `visa/${id}`,
    method: 'post'
  })
}

export function download(id) {
  return request({
    module: 'contract',
    url: `visa/${id}/export`,
    method: 'get',
    responseType: 'blob'
  })
}

export function downloadSettle(id) {
  return request({
    module: 'project',
    url: `foreign/visa-settle/export-settle/${id}`,
    method: 'get',
    responseType: 'blob'
  })
}

export default { get, download, detail }
