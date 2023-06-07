import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/expenseSubject/list',
    method: 'get',
    params
  })
}

export function expenseSubjectAll(params) {
  return request({
    module: 'contract',
    url: 'contract/expenseSubject/listAll',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/expenseSubject/saveOrUpdate',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/expenseSubject/saveOrUpdate',
    method: 'post',
    data: [data]
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'contract/expenseSubject',
    method: 'delete',
    data: ids
  })
}
export default { get, add, edit, del }
