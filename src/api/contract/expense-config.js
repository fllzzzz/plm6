import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'expenseType',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'expenseType',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'expenseType',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'expenseType',
    method: 'delete',
    data: ids
  })
}
export default { get, add, edit, del }