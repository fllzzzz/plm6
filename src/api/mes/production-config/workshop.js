import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'workshop/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'workshop',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'workshop',
    method: 'put',
    data
  })
}

export function editStatus({ id, boolEnabledEnum }) {
  return request({
    module: 'mes',
    url: 'workshop/changeState',
    method: 'put',
    data: { id, boolEnabledEnum }
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'workshop',
    method: 'delete',
    data: {
      ids: ids
    }
  })
}

export default { get, add, edit, del }
