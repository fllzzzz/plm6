import request from '@/utils/request'

// 查询套料成果项目
export function get(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/batch/project',
    method: 'get',
    params
  })
}

// 查询套料批次列表
export function nestingBatchList(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/batch/list',
    method: 'get',
    params
  })
}

// 批次下发
export function nestingBatchIssued(data) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/batch',
    method: 'post',
    data
  })
}

// 删除套料成果
export function nestingBatchDel(data) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/batch',
    method: 'delete',
    data
  })
}

export default { get }
