import request from '@/utils/request'

// 气体统计
export function get(params) {
  return request({
    module: '',
    url: '/api/contract/gas-summary',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: '',
    url: '/api/contract/gas-summary',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: '',
    url: '/api/contract/gas-summary',
    method: 'post',
    data: [data]
  })
}

export function del(ids) {
  return request({
    module: '',
    url: `/api/contract/gas-summary`,
    method: 'delete',
    data: ids
  })
}

// 气体类型列表
export function getGasList(params) {
  return request({
    module: '',
    url: `/api/contract/gas-summary/list-gas-class`,
    method: 'get',
    params
  })
}

export default { get, add, edit, del }
