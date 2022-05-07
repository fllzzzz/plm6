import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'system/assemble-config',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'system/assemble-config',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'system/assemble-config',
    method: 'put',
    data
  })
}

export function del(data) {
  return request({
    module: 'contract',
    url: 'system/assemble-config',
    method: 'delete',
    data
  })
}

export default { get, add, edit, del }
