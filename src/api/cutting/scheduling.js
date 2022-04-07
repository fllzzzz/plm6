import request from '@/utils/request'

// 设备查排产
export function get(params) {
  return request({
    url: `/api/cut/getMachinePlateList`,
    method: 'get',
    params
  })
}

// 项目查排产
export function ads(params) {
  return request({
    url: `/api/cut/getNestingList`,
    method: 'get',
    params
  })
}

// 暂停任务
export function suspendTask(data) {
  return request({
    url: `/api/cut/suspendTask`,
    method: 'post',
    data
  })
}

export default {
  get,
  suspendTask
}
