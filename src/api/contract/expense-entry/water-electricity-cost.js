import request from '@/utils/request'

// 水电费
export function get(params) {
  return request({
    module: '',
    url: '/api/contract/water-electricity',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: '',
    url: '/api/contract/water-electricity',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: '',
    url: '/api/contract/water-electricity',
    method: 'post',
    data: [data]
  })
}

export function del(ids) {
  return request({
    module: '',
    url: `/api/contract/water-electricity`,
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
