import request from '@/utils/request'

// 摊销管理列表
export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/amortization/page',
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

// 摊销分类树
export function amortizationClassTree(params) {
  return request({
    module: 'contract',
    url: `contract/amortizationClass/tree`,
    method: 'get',
    params
  })
}

export default { get, add, edit, del }
