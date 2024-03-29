import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'productionLine/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'productionLine',
    method: 'delete',
    data: {
      ids: ids
    }
  })
}

export function editStatus({ id, boolEnabledEnum }) {
  return request({
    module: 'mes',
    url: 'productionLine/changeState',
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

export function getProductionLineName(params) {
  return request({
    module: 'mes',
    url: 'productionLine/name',
    method: 'get',
    params
  })
}

export default { get, add, edit, del }
