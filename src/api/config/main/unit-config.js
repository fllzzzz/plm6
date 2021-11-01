import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'config',
    url: 'base-config/unit',
    method: 'get',
    params
  })
}

export function batchAdd(data) {
  return request({
    module: 'config',
    url: 'base-config/unit/batch',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'config',
    url: 'base-config/unit',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'config',
    url: 'base-config/unit',
    method: 'delete',
    data: ids
  })
}

export default { get, batchAdd, edit, del }
