import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `install-report/install-statement`,
    method: 'get',
    params
  })
}

// 安装看板
export function installDashboardData(params) {
  return request({
    module: 'project',
    url: `install-report/install-board`,
    method: 'get',
    params
  })
}

// 安装报表汇总
export function installReportSummary(params) {
  return request({
    module: 'project',
    url: `receiving/receiving-install-sum`,
    method: 'get',
    params
  })
}

export default { get }
