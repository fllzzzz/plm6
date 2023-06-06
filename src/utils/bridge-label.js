import { spliceUrl } from '@/utils'
/**
 * TODO:单独开发页面，避免扫码加载标签时，main.js过大，页面加载过慢(main.js小可以考虑合在一起，缩小main.js？)
 * 标签中二维码存储的页面路由路径（path）
 * 之所以路径名称很短，是因为字符串越长，二维码越复杂。
 * 在二维码负责的情况下，在打印标签时，二维码打印面积不够大时，会导致二维码无法正常打印及扫描
 * 路径全称：
 * scan/structure/xxx
 * scan/bridge/xxx
 */
export const QR_SCAN_PATH = {
  ARTIFACT_TASK: '/s/s/a',
  ENCLOSURE_TASK: '/s/s/e',
  AUXILIARY_MATERIAL: '/s/s/m',
  BRIDGE_BOX_TASK: '/s/b/b',
  BRIDGE_SINGLE_ELEMENT_TASK: '/s/b/s',
  BRIDGE_AUXILIARY_MATERIAL: '/s/b/m'
}

// 标签二维码url拼接
export function spliceQrCodeUrl(url, data) {
  let _url = spliceUrl(url, data)
  // TODO: 271 暂时写死,由于标签上的二维码大小有限，避免二维码过于复杂导致二维码无法正常打印。将字符限制为271个。二维码显示内容后期会更改
  if (_url.length > 271) {
    const _data = JSON.parse(JSON.stringify(data))
    delete _data.mn // 制造商名称
    _url = spliceUrl(url, _data)
  }
  return _url
}
