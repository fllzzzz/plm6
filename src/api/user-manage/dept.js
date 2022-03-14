import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'user',
    url: 'dept/list',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'user',
    url: 'dept',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'user',
    url: 'dept',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'user',
    url: 'dept',
    method: 'delete',
    data: ids
  })
}

export function deptTree(params) {
  return request({
    module: 'user',
    url: 'dept/all/simple',
    method: 'get',
    params
  })
}

export default { add, edit, del, get }
