import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `contract/subLedger/listInvoice`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/subLedger/invoice',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/subLedger/invoice',
    method: 'put',
    data: [data]
  })
}

export function del(data) {
  return request({
    module: 'contract',
    url: 'contract/subLedger/invoice',
    method: 'delete',
    data
  })
}

export function audit(data) {
  return request({
    module: 'contract',
    url: `contract/subLedger/invoice/audit`,
    method: 'put',
    data
  })
}
export default { get, add, edit, del }