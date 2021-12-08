import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: `planDetail/list/${params.projectId}`,
    method: 'get',
    params
  })
}


export function edit(data) {
  return request({
    module: 'plan',
    url: 'planDetail/update',
    method: 'put',
    data
  })
}


export default { edit, get }
