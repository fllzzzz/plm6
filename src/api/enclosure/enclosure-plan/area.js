import request from '@/utils/request'

export function allEnclosureProject(params) {
  return request({
    url: '/api/enclosurePlanDetail',
    method: 'get',
    params
  })
}

export function get(id) {
  return request({
    url: `/api/enclosurePlanDetail/tech/${id}`,
    method: 'get'
  })
}

export function add(data) {
  return request({
    url: '/api/enclosurePlanDetail',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    url: '/api/enclosurePlanDetail',
    method: 'put',
    data
  })
}

export function del(id) {
  return request({
    url: `/api/enclosurePlanDetail/${id}`,
    method: 'delete'
  })
}

export default { add, edit, del, get }
