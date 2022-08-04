import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'user',
    url: 'role',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'user',
    url: 'role',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'user',
    url: 'role',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'user',
    url: 'role',
    method: 'delete',
    data: ids
  })
}

export function bindMenu(data) {
  return request({
    module: 'user',
    url: 'role/menu',
    method: 'put',
    data
  })
}

export function roleAll() {
  return request({
    module: 'user',
    url: 'role/all',
    method: 'get'
  })
}

// 获取角色对应的人员及岗位
export function roleBindUser(params) {
  return request({
    module: 'user',
    url: 'role/user',
    method: 'get',
    params
  })
}

// 保存角色对应人员及岗位
export function saveRoleBindUser(data) {
  return request({
    module: 'user',
    url: 'role/update/user',
    method: 'put',
    data
  })
}

// 下载角色已配置人员
export function downloadRoleUser(params) {
  return request({
    module: 'user',
    url: 'role/export',
    responseType: 'blob',
    method: 'get'
  })
}
export default { add, edit, del, get }
