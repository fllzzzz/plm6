import request from '@/utils/request'

// 物业费
export function get(params) {
  return request({
    module: '',
    url: '/api/contract/property-fee',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: '',
    url: '/api/contract/property-fee',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: '',
    url: '/api/contract/property-fee',
    method: 'post',
    data: [data]
  })
}

export function del(ids) {
  return request({
    module: '',
    url: `/api/contract/property-fee`,
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
