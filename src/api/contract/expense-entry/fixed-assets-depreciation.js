import request from '@/utils/request'

// 固定资产折旧
export function get(params) {
  return request({
    module: '',
    url: 'api/contract/fixed-assets-depreciation',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: '',
    url: 'api/contract/fixed-assets-depreciation',
    method: 'post',
    data: [data]
  })
}

export function edit(data) {
  return request({
    module: '',
    url: 'api/contract/fixed-assets-depreciation',
    method: 'put',
    data: [data]
  })
}

export function del(ids) {
  return request({
    module: '',
    url: `api/contract/fixed-assets-depreciation`,
    method: 'delete',
    data: ids
  })
}

// 固定资产折旧修改状态
export function editStatus({ id, boolStatus }) {
  return request({
    module: '',
    url: `api/contract/fixed-assets-depreciation/${id}/${boolStatus}`,
    method: 'delete'
  })
}
export default { get, add, edit, del }
