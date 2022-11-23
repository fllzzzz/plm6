import request from '@/utils/request'

// 切割配置列表
export function get(params) {
  return request({
    module: 'mes',
    url: 'cut',
    method: 'get',
    params
  })
}

// export function batchAdd(data) {
//   return request({
//     module: 'mes',
//     url: 'cut/list',
//     method: 'post',
//     data: data.list
//   })
// }

export function add(data) {
  return request({
    module: 'mes',
    url: 'cut',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'mes',
    url: 'cut',
    method: 'put',
    data
  })
}
export function editHole(data) {
  return request({
    module: 'mes',
    url: 'cut',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'mes',
    url: 'cut/list',
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
