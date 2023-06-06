import request from '@/utils/request'

// 设备折旧折旧
export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/equipmentDepreciation',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/equipmentDepreciation',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/equipmentDepreciation',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: `contract/equipmentDepreciation`,
    method: 'delete',
    data: ids
  })
}

// 设备折旧修改状态
export function editStatus({ id, boolStatus }) {
  return request({
    module: 'contract',
    url: `contract/equipmentDepreciation/${id}/${boolStatus}`,
    method: 'put'
  })
}

// 导入设备
export function listUpload(data) {
  return request({
    module: 'contract',
    url: 'contract/equipmentDepreciation/import',
    responseType: 'blob',
    method: 'post',
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    data
  })
}

// 下载设备折旧模板
export function downloadTemplate(params) {
  return request({
    module: 'contract',
    url: 'contract/equipmentDepreciation/template/export',
    responseType: 'blob',
    method: 'get',
    params
  })
}

export default { get, add, edit, del }
