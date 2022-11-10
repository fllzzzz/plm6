import request from '@/utils/request'

// 公司审批流程列表
export function get(params) {
  return request({
    url: '/api/sys/approve-process',
    method: 'get',
    params,
    cancelKey: false
  })
}

// 新增公司审批流程
export function add(data) {
  return request({
    url: '/api/sys/approve-process',
    method: 'post',
    data
  })
}

// 编辑公司审批流程
export function edit(data) {
  return request({
    url: '/api/sys/approve-process',
    method: 'put',
    data
  })
}

// 获取审核类型
export function getApproveType(params) {
  return request({
    url: '/api/sys/approve-process/code',
    method: 'get',
    params
  })
}

// 编辑公司审批流程
export function del(data) {
  return request({
    url: '/api/sys/approve-process',
    method: 'delete',
    data
  })
}
export default { get, add, edit, del }
