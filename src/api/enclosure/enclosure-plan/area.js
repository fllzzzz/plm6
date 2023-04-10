import request from '@/utils/request'

// 获取所有围护项目
export function allEnclosureProject(params) {
  return request({
    url: '/api/enclosurePlanDetail',
    method: 'get',
    params
  })
}

// 获取围护项目合同量
export function enclosureProjectContent(projectId) {
  return request({
    url: `/api/enclosurePlanDetail/${projectId}`,
    method: 'get'
  })
}

export function get(params) {
  return request({
    url: `/api/enclosurePlanDetail/tech`,
    method: 'get',
    params
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
