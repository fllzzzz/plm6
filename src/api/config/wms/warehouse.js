import request from '@/utils/request'

// 获取仓库位置（简要的）
export function getWarehouseBrief() {
  return request({
    module: 'wms',
    url: 'config/material/warehouse/all/brief',
    method: 'get'
  })
}

export function get(params) {
  return request({
    module: 'wms',
    url: 'config/material/warehouse',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'wms',
    url: 'config/material/warehouse',
    method: 'post',
    data: [data]
  })
}

export function batchAdd(data) {
  return request({
    module: 'wms',
    url: 'config/material/warehouse/batch',
    method: 'post',
    data: data
  })
}

export function edit(data) {
  return request({
    module: 'wms',
    url: `config/material/warehouse`,
    method: 'put',
    data
  })
}

export function editEnabled(data) {
  return request({
    module: 'wms',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: `config/material/warehouse/enabled`,
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'wms',
    url: 'config/material/warehouse',
    method: 'delete',
    data: ids
  })
}

// 获取所有仓库车间名称
export function getWorkshopNameAll(params) {
  return request({
    module: 'wms',
    url: 'config/material/warehouse/ids',
    method: 'get',
    params
  })
}

// 新增车间名称
export function addWorkshop(data) {
  return request({
    module: 'wms',
    url: 'config/material/warehouse/workshop/batch',
    method: 'post',
    data
  })
}

export function delWorkshop(id) {
  return request({
    module: 'wms',
    url: 'config/material/warehouse/workshop',
    method: 'delete',
    data: [id]
  })
}

export default { get, add, batchAdd, edit, del }
