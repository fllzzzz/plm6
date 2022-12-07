import request from '@/utils/request'

export function get() {
  return request({
    module: '',
    url: '/api/scm/SubcontractClass-config',
    method: 'get',
    cancelKey: false
  })
}

export function getSubcontractType() {
  return request({
    module: '',
    url: '/api/scm/SubcontractClass-config/all',
    method: 'get',
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: '',
    url: '/api/scm/SubcontractClass-config',
    method: 'post',
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: '',
    url: '/api/scm/SubcontractClass-config',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: '',
    url: '/api/scm/SubcontractClass-config',
    method: 'delete',
    data: ids
  })
}
export default { get, add, edit, del }
