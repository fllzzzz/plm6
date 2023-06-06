import request from '@/utils/request'

export function deliveryCargoList(params) {
  return request({
    module: 'project',
    url: `receiving/listProduct/${params.cargoListId}/print`,
    method: 'get',
    params
  })
}

export function deliveryReportList(params) {
  return request({
    module: 'project',
    url: `receiving/receiving-statement/print`,
    method: 'get',
    params
  })
}

export function deliveryInstallList(params) {
  return request({
    module: 'project',
    url: `receiving/receiving-install-statement/print`,
    method: 'get',
    params
  })
}

export function installReportList(params) {
  return request({
    module: 'project',
    url: `install-report/install-statement/print`,
    method: 'get',
    params
  })
}

export default {
  deliveryCargoList, // 自制收货记录
  deliveryReportList, // 收货报表
  deliveryInstallList, // 收安报表
  installReportList // 安装报表
}
