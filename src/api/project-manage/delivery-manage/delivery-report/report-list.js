import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `receiving/receiving-statement`,
    method: 'get',
    params
  })
}

// 收货看板
export function deliveryDashboardData(params) {
  return request({
    module: 'project',
    url: `receiving/receiving-board`,
    method: 'get',
    params
  })
}

// 收安报表
export function deliveryInstallData(params) {
  return request({
    module: 'project',
    url: `receiving/receiving-install-statement`,
    method: 'get',
    params
  })
}

// 收安报表汇总
export function deliveryInstallSummary(params) {
  return request({
    module: 'project',
    url: `receiving/receiving-install-sum`,
    method: 'get',
    params,
    cancelKey: false
  })
}
export default { get }
