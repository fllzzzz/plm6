import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: `planDetail/list/${params.projectId}`,
    method: 'get',
    params
  })
}

// export function edit(data) {
//   return request({
//     module: 'plan',
//     url: 'planDetail/update',
//     method: 'put',
//     data
//   })
// }

// 批量修改
export function edit(areaId, data) {
  return request({
    module: 'plan',
    url: `planDetail/updateList/${areaId}`,
    method: 'put',
    data
  })
}

export default { edit, get }
