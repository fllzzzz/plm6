import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'system/structure-classification',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'system/structure-classification',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'system/structure-classification',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'system/structure-classification',
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
