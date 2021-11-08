import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'productionLine/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'delete',
    data: {
      ids: ids
    }
  })
}

export function editStatus({ id, boolEnabledEnum }) {
  return request({
    module: 'mes',
    url: 'productionLine/changeState',
    method: 'put',
    data: { id, boolEnabledEnum }
  })
}

export default { get, add, edit, del }
