import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `contract/payment/list`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'project',
    url: `sub-payment`,
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'project',
    url: `sub-payment`,
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'contract/payment/del-payment',
    method: 'delete',
    data: ids
  })
}

export function settleSave(data) {
  return request({
    module: 'project',
    url: `sub-payment/settle`,
    method: 'post',
    data
  })
}

export function settleEdit(data) {
  return request({
    module: 'project',
    url: `sub-payment/settle`,
    method: 'put',
    data
  })
}

export function editStatus(data) {
  return request({
    module: 'project',
    url: `sub-payment/check-payment`,
    method: 'put',
    data
  })
}

export function check(params) {
  return request({
    module: 'project',
    url: `sub-payment/check-settle`,
    method: 'put',
    params
  })
}
export default { add, edit, get, del }
