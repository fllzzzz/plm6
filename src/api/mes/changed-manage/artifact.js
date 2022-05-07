import request from '@/utils/request'

export function changeStatus(id) {
  return request({
    module: 'mes',
    url: `abnormal/${id}`,
    method: 'put'
  })
}

export function partChange(id) {
  return request({
    module: 'mes',
    url: `abnormal/handle/part`,
    method: 'put',
    data: {
      id
    }
  })
}

export function exceptionChange(data) {
  return request({
    module: 'mes',
    url: 'abnormal/artifact',
    method: 'put',
    data
  })
}

export function taskChange(data) {
  return request({
    module: 'mes',
    url: 'abnormal/artifact/task',
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
