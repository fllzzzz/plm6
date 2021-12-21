import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'deploy/project/template',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'deploy/project/templateSave',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'deploy/project/template/Update',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `deploy/project/remove/${id}`,
    method: 'delete'
  })
}

export function editStatus(data) {
  return request({
    module: 'contract',
    url: 'deploy/project/status',
    method: 'post',
    data
  })
}

// 获取所有项目用户
export function getUserAllSimpleByProject(params) {
  return request({
    module: 'contract',
    url: `deploy/project/template/details`,
    method: 'get',
    params
  })
}

/**
 * 修改默认模板
 * @param {number} id | required 表格id
 * @param {Boolean} isDefault | required 是否默认模板
 */
export function editDefault({ id, isDefault }) {
  return request({
    module: 'contract',
    url: `deploy/project/${id}/default/${isDefault}`,
    method: 'put'
  })
}

export default { get, add, edit, del }