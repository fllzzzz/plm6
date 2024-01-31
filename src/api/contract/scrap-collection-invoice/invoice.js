import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/waste',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/invoice/waste',
    method: 'post',
    params: { projectId: data.projectId },
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/invoice/waste',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `contract/invoice/deleteById/${id}/waste`,
    method: 'delete'
  })
}

export function editStatus(data) {
  return request({
    module: 'contract',
    url: `contract/invoice/audit/waste`,
    method: 'put',
    data
  })
}

export default { get, add, edit, del }
