import request from '@/utils/request'

export function get() {
  return request({
    module: 'project',
    url: 'qhse-problem-type',
    method: 'get',
    cancelKey: false
  })
}

export function getQualityProblemType() {
  return request({
    module: 'project',
    url: 'qhse-problem-type/all',
    method: 'get',
    cancelKey: false
  })
}

export function add(data) {
  return request({
    module: 'project',
    url: 'qhse-problem-type',
    method: 'post',
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'project',
    url: 'qhse-problem-type',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'project',
    url: 'qhse-problem-type',
    method: 'delete',
    data: ids
  })
}
export default { get, add, edit, del }
