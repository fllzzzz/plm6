import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `install-report/list-receiving`,
    method: 'get',
    params
  })
}

// 安装填报
export function installSave(data) {
  return request({
    module: 'project',
    url: `install-report/save`,
    method: 'post',
    data
  })
}

export default { get }
