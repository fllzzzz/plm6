import request from '@/utils/request'

// 型材套料：项目套料进度
export function get(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/project/status',
    method: 'get',
    params
  })
}

// 查询项目套料进度详情
export function getProjectNesting(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/project/status/detail',
    method: 'get',
    params
  })
}
// 查看查询区域部件清单列表
export function getAssembleList(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/area/status/detail',
    method: 'get',
    params
  })
}
// 查询区域部件缺少NC1清单数量
export function getNoFileList(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/area/status/number',
    method: 'get',
    params
  })
}

export default { get }
