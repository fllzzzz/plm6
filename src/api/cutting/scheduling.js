import request from '@/utils/request'
// import { continueTask } from './project-data'

// 设备查排产
export function get(params) {
  return request({
    url: `/api/cut/getMachinePlateList`,
    method: 'get',
    params
  })
}

export function ads(params) {
  return request({
    url: `/api/cut/getNestingList`,
    method: 'get',
    params
  })
}

// 暂停
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
