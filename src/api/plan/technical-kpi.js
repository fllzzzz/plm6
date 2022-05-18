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

// 获取所有项目的深化量和计划量
export function getProjectYearSummary(params) {
  return request({
    module: 'plan',
    url: 'deepenRecord/list',
    method: 'get',
    params
  })
}

// 获取全年每个月的深化量
export function getMonthMete(params) {
  return request({
    module: 'plan',
    url: 'deepenRecord/listMonthMete',
    method: 'get',
    params
  })
}

// 获取每个项目的深化量
export function getProjectMete(params) {
  return request({
    module: 'plan',
    url: 'deepenRecord/listProjectMete',
    method: 'get',
    params
  })
}

// 获取全年每个人的深化量
export function getUserYearMete(params) {
  return request({
    module: 'plan',
    url: 'deepenRecord/listUserMete',
    method: 'get',
    params
  })
}

// 获取每个人当月和上月的深化量
export function getUserMonthMete(params) {
  return request({
    module: 'plan',
    url: 'deepenRecord/listUserMonthMete',
    method: 'get',
    params
  })
}

