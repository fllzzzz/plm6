import request from '@/utils/request'

export function get() {
  return request({
    module: 'contract',
    url: 'deploy/project/template',
    method: 'get'
  })
}

// 获取模板里所有用户
export function getUserByTemplate(params) {
  return request({
    module: 'contract',
    url: 'deploy/project/template/details',
    method: 'get',
    params
  })
}
export default { get }