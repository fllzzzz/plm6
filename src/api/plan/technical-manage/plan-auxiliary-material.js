import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: `auxiliaryMaterial/listPage`,
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'plan',
    url: 'auxiliaryMaterial/update',
    method: 'put',
    data
  })
}

export function add(data) {
  return request({
    module: 'plan',
    url: 'auxiliaryMaterial/save',
    method: 'post',
    params: { projectId: data.projectId, monomerId: data.monomerId },
    data: data.list
  })
}

export function del(projectId, data) {
  return request({
    module: 'plan',
    url: `auxiliaryMaterial/delete`,
    method: 'delete',
    params: { projectId: projectId },
    data
  })
}

export default { get, edit, del, add }
