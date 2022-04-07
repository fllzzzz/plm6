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
    module: 'contract',
    url: 'contract/payment/save',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: `contract/payment/update`,
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
    module: 'contract',
    url: 'contract/supplier-settlement/save',
    method: 'post',
    data
  })
}

export function settleConfirm(params) {
  return request({
    module: 'contract',
    url: 'contract/supplier-settlement/check',
    method: 'put',
    params
  })
}
export default { add, edit, del, get }
