import request from '@/utils/request'

// 获取项目统计数量
export function getProjectSummary(params) {
  return request({
    module: 'project',
    url: 'projectStatistics',
    method: 'get',
    params: params
  })
}

// 获取每月发运车次
export function getCargoList(params) {
  return request({
    module: 'bridge',
    url: 'kpi/production/cargoList',
    method: 'get',
    params: params
  })
}

// 目标达成
export function getTargetComplete(params) {
  return request({
    module: 'bridge',
    url: 'kpi/production/complete/year',
    method: 'get',
    params: params
  })
}

// 获取每月生产量
export function getYieldList(params) {
  return request({
    module: 'bridge',
    url: 'kpi/production/complete/yield',
    method: 'get',
    params: params
  })
}

// 获取每月生产线产量
export function getLineYieldList(params) {
  return request({
    module: 'bridge',
    url: 'kpi/production/production_line/complete/yield',
    method: 'get',
    params: params
  })
}

// 获取项目生产信息
export function getProjectYieldList(params) {
  return request({
    module: 'bridge',
    url: 'kpi/production/project/complete/yield',
    method: 'get',
    params: params
  })
}

// 获取当年问题上报数量按类型分组
export function getQHSEList(params) {
  return request({
    module: 'bridge',
    url: 'kpi/production/qhse/type',
    method: 'get',
    params: params
  })
}

// 查询质量问题上报数量按问题原因分组
export function getQHSEQualityList(params) {
  return request({
    module: 'bridge',
    url: 'kpi/production/qhse/type/quality',
    method: 'get',
    params: params
  })
}
