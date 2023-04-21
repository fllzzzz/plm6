import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/mes/enclosure/productionLine/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    url: '/api/mes/enclosure/productionLine',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    url: '/api/mes/enclosure/productionLine',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    url: '/api/mes/enclosure/productionLine',
    method: 'delete',
    data: {
      ids: ids
    }
  })
}

export function editStatus({ id, boolEnabledEnum }) {
  return request({
    url: '/api/mes/enclosure/productionLine/changeState',
    method: 'put',
    data: { id, boolEnabledEnum }
  })
}

export function getProductionLineName(params) {
  return request({
    url: '/api/mes/enclosure/productionLine/name',
    method: 'get',
    params
  })
}

export default { get, add, edit, del }
