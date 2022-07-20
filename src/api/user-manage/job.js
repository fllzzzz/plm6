import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'user',
    url: 'job',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'user',
    url: 'job',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'user',
    url: 'job',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'user',
    url: 'job',
    method: 'delete',
    data: ids
  })
}

export function editStatus(params) {
  return request({
    module: 'user',
    url: 'job/enabled',
    method: 'put',
    params
  })
}

export function jobAll(params) {
  return request({
    module: 'user',
    url: 'job/all',
    method: 'get',
    params
  })
}

// 岗位 批量人员设置
export function saveJobUser(data) {
  return request({
    module: 'user',
    url: 'job/save/user',
    method: 'post',
    data
  })
}

// 获取岗位已配置人员
export function getJobUser(params) {
  return request({
    module: 'user',
    url: 'job/query/user',
    method: 'get',
    params
  })
}

// 下载岗位已配置人员
export function downloadJobUser(params) {
  return request({
    module: 'user',
    url: 'job/export',
    responseType: 'blob',
    method: 'get',
    params
  })
}
export default { add, edit, del, get }
