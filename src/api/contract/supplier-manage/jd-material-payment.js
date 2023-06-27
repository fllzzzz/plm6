import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `contract/purchaseLedger/listPayment`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/purchaseLedger',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/purchaseLedger',
    method: 'put',
    data: [data]
  })
}

export function del(data) {
  return request({
    module: 'contract',
    url: 'contract/purchaseLedger',
    method: 'delete',
    data
  })
}

export function audit(data) {
  return request({
    module: 'contract',
    url: `contract/purchaseLedger/audit`,
    method: 'put',
    data
  })
}
export default { get, add, edit, del }
