import request from '@/utils/request'

export function getCondition(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/condition',
    method: 'get',
    params
  })
}

export function get(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/list',
    method: 'get',
    params
  })
}

export function getNestingSummary(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/summary',
    method: 'get',
    params
  })
}

export function setNotNeedNesting(data) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/no',
    method: 'post',
    data
  })
}

export function getNotNeedNesting(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/no',
    method: 'get',
    params
  })
}

export function delNotNeedNesting(data) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/no',
    method: 'delete',
    data
  })
}
// 型材套料
export function extrusionNesting(data) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait',
    method: 'post',
    data
  })
}
// 套料成果
export function nestingProgress(params) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/batch',
    method: 'get',
    params
  })
}

export function delNestingResult(data) {
  return request({
    module: 'mes',
    url: 'section_steel/nesting/wait/batch',
    method: 'delete',
    data
  })
}

export default { get }
