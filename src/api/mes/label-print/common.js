import request from '@/utils/request'

/**
 * 获取某项目下产品标签打印配置
 * @export
 * @param {number} projectId|required 项目id
 * @returns
 */
export function getPrintConfig(projectId) {
  return request({
    module: 'mes',
    url: 'print/config',
    method: 'get',
    params: { projectId }
  })
}

/**
 * 更新某项目下产品标签打印配置
 * @export
 * @param {number} projectId|required 项目id
 * @param {number} weight|required 重量 0净重 1毛重 null不显示
 * @param {number} copiesQuantity|required 打印份数
 * @param {number} showProductionLine|required 显示生产线(1显示 0不显示)
 * @param {string} manufacturerName|required 制造商
 * @returns
 */
export function setPrintConfig({ projectId, weight, copiesQuantity, printAll, showProductionLine, manufacturerName, showArea, showMonomer }) {
  return request({
    module: 'mes',
    url: 'print/config',
    method: 'post',
    data: { projectId, weight, copiesQuantity, printAll, showProductionLine, manufacturerName, showArea, showMonomer }
  })
}
