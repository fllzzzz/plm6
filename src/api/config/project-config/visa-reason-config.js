import request from '@/utils/request'

export function get() {
  return request({
    module: 'project',
    url: 'visa-reason',
    method: 'get',
    cancelKey: false
  })
}

export function getVisaReason() {
  return request({
    module: 'project',
    url: 'visa-reason/all',
    method: 'get',
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'project',
    url: 'visa-reason',
    method: 'post',
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'project',
    url: 'visa-reason',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'project',
    url: 'visa-reason',
    method: 'delete',
    data: ids
  })
}
export default { get, add, edit, del }
