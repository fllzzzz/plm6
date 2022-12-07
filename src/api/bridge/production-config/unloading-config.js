import request from '@/utils/request'

// 切割配置列表/下料方式配置

export function get(params) {
  return request({
    module: 'bridge',
    url: 'cut/list/laying',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'bridge',
    url: 'cut/list/laying',
    method: 'post',
    data
  })
}
export function edit(data) {
  return request({
    module: 'bridge',
    url: 'cut/list/laying',
    method: 'put',
    data
  })
}
export function del(ids) {
  return request({
    module: 'bridge',
    url: 'cut/list/laying',
    method: 'delete',
    data: ids
  })
}
// export function batchUnloadingAdd(data) {
//   return request({
//     module: 'bridge',
//     url: 'cut/list/laying',
//     method: 'post',
//     data
//   })
// }
// export function batchUnloading() {
//   return request({
//     module: 'bridge',
//     url: 'cut/list/laying',
//     method: 'get'
//   })
// }

export function addCutConfigDetail(data) {
  return request({
    module: 'bridge',
    url: 'cut/detail',
    method: 'post',
    data
  })
}

export function editCutConfigDetail(data) {
  return request({
    module: 'bridge',
    url: 'cut/detail',
    method: 'put',
    data
  })
}

export function delCutConfigDetail(ids) {
  return request({
    module: 'bridge',
    url: 'cut/detail',
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
