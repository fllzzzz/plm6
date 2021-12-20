import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/payment/listPage',
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

//审核
export function editStatus(params) {
  return request({
    module: 'contract',
    url: `contract/payment/audit`,
    method: 'put',
    params
  })
}

export function paySum() {
  return request({
    module: 'contract',
    url: 'contract/payment/sum',
    method: 'get'
  })
}

export default { get, add }