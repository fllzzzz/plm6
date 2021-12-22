import request from '@/utils/request'

export function change(data) {
  return request({
    module: 'mes',
    url: 'abnormal/assemble/task_change',
    method: 'put',
    data
  })
}
