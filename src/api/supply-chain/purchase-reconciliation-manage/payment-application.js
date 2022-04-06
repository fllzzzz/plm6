import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `supply-chain/order-payment/listPayment/${params.orderId}`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'supply-chain/order-payment/save-payment',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: `supply-chain/order-payment/update-payment`,
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'supply-chain/order-payment/del-payment',
    method: 'delete',
    data: ids
  })
}

export function settleSave(data) {
  return request({
    module: 'contract',
    url: 'supply-chain/order-payment/save-settlement',
    method: 'post',
    data
  })
}

export function settleConfirm(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/order-payment/check-settlement',
    method: 'put',
    params
  })
}
export default { add, edit, del, get }
