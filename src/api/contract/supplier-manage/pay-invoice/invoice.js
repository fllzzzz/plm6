import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `contract/receive-invoice/${params.orderId}`,
    method: 'get'
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/receive-invoice',
    method: 'post',
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/receive-invoice',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    module: 'contract',
    url: `contract/receive-invoice/${id}`,
    method: 'delete'
  })
}

// 审核
export function editStatus(params) {
  return request({
    module: 'contract',
    url: `contract/receive-invoice/audit`,
    method: 'put',
    params
  })
}

// export function receiveSum() {
//   return request({
//     module: 'contract',
//     url: 'contract/receive-invoice/sum',
//     method: 'get'
//   })
// }

export default { get, add, edit, del }
