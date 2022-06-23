import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'process/group',
    method: 'get',
    params
  })
}

export function getByProductType(params) {
  return request({
    module: 'mes',
    url: 'process',
    method: 'get',
    params
  })
}

// 修改工序上报类型
export function editInspection(data) {
  return request({
    module: 'mes',
    url: 'process',
    method: 'put',
    data
  })
}

// 设置工序价格
export function editWage(data) {
  return request({
    module: 'mes',
    url: 'process/wage',
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
    module: 'mes',
    url: 'process/save/list',
    method: 'post',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'process',
    method: 'delete',
    data: { ids }
  })
}

export default { get, add: edit, edit, del }
