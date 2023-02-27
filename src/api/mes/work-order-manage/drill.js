
import request from '@/utils/request'

/**
 * @description: 获取钻孔工单项目信息
*/
export function getProjectInfo(params) {
  return request({
    module: 'mes',
    url: `task/order/machinePart/project/list`,
    method: 'get',
    params
  })
}

