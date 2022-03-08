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

// 获取当前项目的项目模式
export function modeData(data) {
  return request({
    module: 'contract',
    url: 'config/getProjectMode',
    method: 'get'
  })
}

// 获取当前用户可以更改的项目模式
export function modeList(data) {
  return request({
    module: 'contract',
    url: 'config/listProjectMode',
    method: 'get'
  })
}
export default { get, edit }
