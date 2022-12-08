import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'bridge',
    url: 'inspectionTeam/page',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'bridge',
    url: 'inspectionTeam',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'bridge',
    url: 'inspectionTeam',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'bridge',
    url: 'inspectionTeam',
    method: 'delete',
    data: { ids }
  })
}

export default { get, add, edit, del }
