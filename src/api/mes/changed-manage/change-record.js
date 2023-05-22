import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'tech/change/page',
    method: 'get',
    params
  })
}

export function changeDetail(id) {
  return request({
    module: 'mes',
    url: `tech/change/${id}`,
    method: 'get'
  })
}

export function taskDetail(id) {
  return request({
    module: 'mes',
    url: `tech/change/${id}/task`,
    method: 'get'
  })
}

export function getChangeTaskList(id) {
  return request({
    module: 'mes',
    url: `tech/change/${id}/task/issued/temporary`,
    method: 'get'
  })
}

export function handleChange(data) {
  return request({
    module: 'mes',
    url: `tech/change/task/handle/temporary`,
    method: 'put',
    data
  })
}

export default { get }
