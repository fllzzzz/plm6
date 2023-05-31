import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `receiving/listReceiving`,
    method: 'get',
    params
  })
}

// 确认收货
export function edit(data) {
  return request({
    module: 'project',
    url: `receiving/confirmReceiving`,
    method: 'post',
    data
  })
}

// 获取收货列表
export function deliveryProductList(cargoListId, params) {
  return request({
    module: 'project',
    url: `receiving/listProduct/${cargoListId}`,
    method: 'get',
    params
  })
}

export default { edit, get }
