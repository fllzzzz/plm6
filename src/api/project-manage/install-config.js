import request from '@/utils/request'

// 安装填报方式修改
export function editInstall(data) {
  return request({
    module: 'project',
    url: `install-report/updateReportMethod`,
    method: 'put',
    data
  })
}

// 获取项目下配套件列表
export function get(params) {
  return request({
    module: '',
    url: `/api/plan/auxiliaryMaterial/class-list`,
    method: 'get',
    params
  })
}

// 修改项目下配套件是否即入即安
export function editAuxiliaryReportMethod(list) {
  return request({
    module: 'project',
    url: `install-report/auxiliaryMaterial-report`,
    method: 'put',
    data: list
  })
}
export default { get }
