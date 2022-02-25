import request from '@/utils/request'

// 获取备料列表
export function get(params) {
  return request({
    module: 'plan',
    url: 'material-preparation/project-preparation',
    method: 'get',
    params
  })
}

// 获取备料列表
export function detail(id) {
  return request({
    module: 'plan',
    url: `material-preparation/project-preparation/${id}`,
    method: 'get'
  })
}

// 编辑备料清单
export function edit(data) {
  return request({
    module: 'plan',
    url: 'material-preparation/project-preparation',
    method: 'put',
    data
  })
}

// 获取未完工的项目备料范围列表
export function getProjectListForRangeInfo() {
  return request({
    module: 'plan',
    url: 'material-preparation/project-preparation/projects/range-info',
    method: 'get'
  })
}

/**
 * 设置未完工的项目备料范围列表
 * 只可修改未备料的项目列表
 * @param {*} params
 * @returns
 */
export function setProjectListForRangeInfo(data) {
  return request({
    module: 'plan',
    url: 'material-preparation/project-preparation/projects/range-info',
    method: 'put',
    data
  })
}

export default { get, detail, edit }
