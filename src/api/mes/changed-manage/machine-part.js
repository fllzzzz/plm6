import request from '@/utils/request'

export function change(data) {
  return request({
    module: 'mes',
    url: 'abnormal/machine_part/task_change',
    method: 'put',
    data
  })
}

export function taskList(params) {
  return request({
    module: 'mes',
    url: 'abnormal/machine_part/task',
    method: 'get',
    params
  })
}
