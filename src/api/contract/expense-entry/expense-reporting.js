import request from '@/utils/request'

// 费用填报
export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/expense-reimburse',
    method: 'get',
    params
  })
}

// 获取费用类别
export function getExpenseType(params) {
  return request({
    module: 'contract',
    url: 'expenseType/all',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/expense-reimburse',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/expense-reimburse',
    method: 'put',
    data: data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: `contract/expense-reimburse`,
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
