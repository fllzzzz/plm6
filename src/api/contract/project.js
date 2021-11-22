import request from '@/utils/request'

// 查询项目信息（项目按年份区别）
export function getProjectGroupByYear(params) {
  return request({
    module: 'contract',
    url: 'project/simple',
    method: 'get',
    params
  })
}

// 查询项目信息（用户/项目按年份区别）
export function getUserProjects(params) {
  return request({
    module: 'contract',
    url: 'user/project/simple',
    method: 'get',
    params
  })
}
