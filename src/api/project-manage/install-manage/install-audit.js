import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `install-report/list-group`,
    method: 'get',
    params
  })
}

// 安装填报审核明细
export function installDetail(params) {
  return request({
    module: 'project',
    url: `install-report/list-report`,
    method: 'get',
    params
  })
}

// 安装填报审核
export function installAuditSave(data) {
  return request({
    module: 'project',
    url: `install-report/check`,
    method: 'post',
    data
  })
}

export default { get }
