import request from '@/utils/request'

// 人员工资
export function get(params) {
  return request({
    module: '',
    url: '/api/contract/wage-summary',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: '',
    url: '/api/contract/wage-summary',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: '',
    url: '/api/contract/wage-summary',
    method: 'post',
    data: [data]
  })
}

export function del(ids) {
  return request({
    module: '',
    url: `/api/contract/wage-summary`,
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
