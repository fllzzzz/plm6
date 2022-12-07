import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/mes/bridge/productionLine/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    url: '/api/mes/bridge/productionLine',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    url: '/api/mes/bridge/productionLine',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    url: '/api/mes/bridge/productionLine',
    method: 'delete',
    data: {
      ids: ids
    }
  })
}

export function editStatus({ id, boolEnabledEnum }) {
  return request({
    url: '/api/mes/bridge/productionLine/changeState',
    method: 'put',
    data: { id, boolEnabledEnum }
  })
}

export function productConfigInfo({ productType, productionLineTypeEnum }) {
  return request({
    module: 'mes',
    url: 'productionLine/type',
    method: 'get',
    params: { productType, productionLineTypeEnum }
  })
}

export default { get, add, edit, del }
