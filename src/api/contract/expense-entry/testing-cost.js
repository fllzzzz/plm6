import request from '@/utils/request'

// // 检测费
export function getDetail(params) {
  return request({
    module: '',
    url: '/api/contract/testing-fee',
    method: 'get',
    params
  })
}

// 检测费汇总列表
export function get(params) {
  return request({
    module: '',
    url: '/api/contract/testing-fee/summary',
    method: 'get',
    params
  })
}

// 获取检测费用类别
export function getTestingFee(params) {
  return request({
    module: '',
    url: '/api/system/testing-fee-type/all',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: '',
    url: '/api/contract/testing-fee',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: '',
    url: '/api/contract/testing-fee',
    method: 'put',
    data: [data]
  })
}

export function del(ids) {
  return request({
    module: '',
    url: `/api/contract/testing-fee`,
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
