import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `contract/payment/list`,
    method: 'get',
    params
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

export function orderInfo(orderProperty) {
  return request({
    module: 'contract',
    url: `contract/payment/listOrdersByProperty/${orderProperty}`,
    method: 'get'
  })
}

// 审核
export function editStatus(data) {
  return request({
    module: 'contract',
    url: `contract/payment/audit`,
    method: 'put',
    data
  })
}

export function paySum() {
  return request({
    module: 'contract',
    url: 'contract/payment/sum',
    method: 'get'
  })
}

// 付款详情
export function payDetail(paymentId) {
  return request({
    module: 'contract',
    url: `contract/payment/${paymentId}`,
    method: 'get',
    cancelKey: false
  })
}

export default { get, add }
