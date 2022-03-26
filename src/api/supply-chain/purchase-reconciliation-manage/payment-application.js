import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `supply/chain/order/payment/${params.orderId}`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'supply/chain/order/payment',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: `supply/chain/order/payment/${data.id}`,
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'supply/chain/order/payment',
    method: 'delete',
    data: ids
  })
}

export function settleSave(data) {
  return request({
    module: 'contract',
    url: 'supply/chain/order/payment/settlement',
    method: 'post',
    data
  })
}
export default { add, edit, del, get }
