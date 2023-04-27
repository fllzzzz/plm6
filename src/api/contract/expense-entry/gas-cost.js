import request from '@/utils/request'

// 气体统计
export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/gas/list',
    method: 'get',
    params
  })
}

// 新增
export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/gas/saveOrUpdate',
    method: 'post',
    data
  })
}

// 编辑
export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/gas/saveOrUpdate',
    method: 'post',
    data
  })
}

// 删除
export function del(ids) {
  return request({
    module: 'contract',
    url: 'contract/gas',
    method: 'delete',
    data: ids
  })
}

// 新增气体时需要的起始、结束日期
export function getDate(params) {
  return request({
    module: 'contract',
    url: 'contract/gas/getDate',
    method: 'get',
    params
  })
}

// 气体类型树
export function getGasTree(params) {
  return request({
    module: 'contract',
    url: `contract/gas/gasTree`,
    method: 'get',
    params
  })
}

export default { get, add, edit, del }
