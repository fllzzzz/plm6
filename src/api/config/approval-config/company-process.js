import request from '@/utils/request'

// 公司审批流程列表
export function get(params) {
  return request({
    url: '/api/sys/approve-process',
    method: 'get',
    params
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

export default { get, edit }
