import request from '@/utils/request'

// 厂房折旧
export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/fixed-assets-depreciation',
    method: 'get',
    params
  })
}

// 新增
export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/fixed-assets-depreciation',
    method: 'post',
    data: [data]
  })
}

// 编辑
export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/fixed-assets-depreciation',
    method: 'put',
    data: [data]
  })
}

// 删除
export function del(ids) {
  return request({
    module: 'contract',
    url: `contract/fixed-assets-depreciation`,
    method: 'delete',
    data: ids
  })
}

// 修改状态
export function editStatus({ id, boolStatus }) {
  return request({
    module: 'contract',
    url: `contract/fixed-assets-depreciation/${id}/${boolStatus}`,
    method: 'put'
  })
}
export default { get, add, edit, del }
