import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'change/reason',
    method: 'get',
    params,
    cancelKey: false
  })
}

export function getChangeReasonConfig(params) {
  return request({
    module: 'contract',
    url: 'change/reason/all',
    method: 'get',
    params,
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'change/reason',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'change/reason',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'change/reason',
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
