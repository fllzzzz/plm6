import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: `supply-chain/logistics-payment/list`,
    method: 'get',
    params,
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'supply-chain/logistics-payment/save',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: `supply-chain/logistics-payment/update`,
    method: 'post',
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

// 获取根据合同主体和物流公司id查询可付款明细
export function payableList(params) {
  return request({
    module: 'contract',
    url: 'supply-chain/logistics-payment/list-info',
    method: 'get',
    params
  })
}
export default { add, edit, del, get }
