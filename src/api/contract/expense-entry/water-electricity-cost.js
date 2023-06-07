import request from '@/utils/request'

// 水电费
export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/waterElectricity/list',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/waterElectricity/saveOrUpdate',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/waterElectricity/saveOrUpdate',
    method: 'post',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: `contract/waterElectricity`,
    method: 'delete',
    data: ids
  })
}

// 获取新增时水电费需要的起始、结束日期
export function getDate(params) {
  return request({
    module: 'contract',
    url: 'contract/waterElectricity/getDate',
    method: 'get',
    params
  })
}

export default { get, add, edit, del }
