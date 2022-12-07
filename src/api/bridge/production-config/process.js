import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/mes/bridge/process/group',
    method: 'get',
    params
  })
}

export function getByProductType(params) {
  return request({
    url: '/api/mes/bridge/process',
    method: 'get',
    params
  })
}

// 修改工序上报类型
export function editInspection(data) {
  return request({
    url: '/api/mes/bridge/process',
    method: 'put',
    data
  })
}

// 设置工序价格
export function editWage(data) {
  return request({
    url: '/api/mes/bridge/process/wage',
    method: 'post',
    data
  })
}

// export function add(data) {
//   return request({
//     module: 'mes',
//     url: 'process',
//     method: 'post',
//     data
//   })
// }

export function edit(data) {
  return request({
    url: '/api/mes/bridge/process/save/list',
    method: 'post',
    data
  })
}

export function del(ids) {
  return request({
    url: '/api/mes/bridge/process',
    method: 'delete',
    data: { ids }
  })
}

export default { get, add: edit, edit, del }
