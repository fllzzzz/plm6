import request from '@/utils/request'

// 获取钢材对应材料配置的简要信息
export function getSteelClassifyConfBrief() {
  return request({
    module: 'config',
    url: 'structure-steel/classification/all/brief',
    method: 'get'
  })
}

// TODO:修改url并增加module
export function get(params) {
  return request({
    url: '/api/steel/classification',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    url: '/api/steel/classification',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    url: '/api/steel/classification',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    url: '/api/steel/classification',
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
