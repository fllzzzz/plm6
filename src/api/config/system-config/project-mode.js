import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'project/listAllProject',
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'config/setProjectMode',
    method: 'put',
    data
  })
}

export function modeData(data) {
  return request({
    module: 'contract',
    url: 'config/getProjectMode',
    method: 'get'
  })
}

export default { get, edit }
