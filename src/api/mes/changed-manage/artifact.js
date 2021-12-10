import request from '@/utils/request'

export function changeStatus(id) {
  return request({
    module: 'mes',
    url: `abnormal/${id}`,
    method: 'put'
  })
}

export function change(data) {
  return request({
    module: 'mes',
    url: 'abnormal/artifact',
    method: 'put',
    data
  })
}

export function exceptionList(params) {
  return request({
    module: 'mes',
    url: 'abnormal/report',
    method: 'get',
    params
  })
}

export function extraTaskList(params) {
  return request({
    module: 'mes',
    url: 'changed_list/extra_task',
    method: 'get',
    params
  })
}
