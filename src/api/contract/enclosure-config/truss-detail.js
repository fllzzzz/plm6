import request from '@/utils/request'
import { TechnologyTypeEnum } from '@enum-ms/contract'

export function get(params) {
  return request({
    module: 'contract',
    url: 'enclosure/dictionaries/dictDetail/all',
    method: 'get',
    params
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: `enclosure/dictionaries/dictDetailSave/type/${TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V}`,
    method: 'post',
    data
  })
}

export default { get, edit }
