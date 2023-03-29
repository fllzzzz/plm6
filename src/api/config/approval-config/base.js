import request from '@/utils/request'

// 获取审批配置
export function getApprovalConf() {
  return request({
    url: '/api/sys/approve-process/status',
    method: 'get'
  })
}

// 保存审批配置
export function setApprovalConf(data) {
  return request({
    url: '/api/sys/approve-process/status',
    method: 'put',
    data
  })
}
