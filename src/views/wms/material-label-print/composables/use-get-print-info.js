import { detail } from '@/api/wms/material-label-print/receipt-mode'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

export default function useGetPrintInfo() {
  // 加载详情列
  // async function getDetailMaterialList(detailId) {
  //   let { materialList = [] } = await detail(detailId)
  //   await setSpecInfoToList(materialList)
  //   materialList = await numFmtByBasicClass(materialList, {
  //     toSmallest: false,
  //     toNum: false
  //   })
  //   materialList.forEach((row) => {
  //     if (row.basicClass === matClsEnum.STEEL_COIL.V) {
  //       row.printNumber = 1
  //     } else {
  //       row.printNumber = row.quantity
  //     }
  //   })
  //   return materialList
  // }

  async function getDetailMaterialList(detailIds) {
    if (!Array.isArray(detailIds)) {
      detailIds = [detailIds]
    }
    const allInterFace = []
    const materialList = []
    for (const id of detailIds) {
      const ps = detail(id).then((data) => {
        if (data && data.materialList) {
          materialList.push.apply(materialList, data.materialList)
        }
      })
      allInterFace.push(ps)
    }
    await Promise.all(allInterFace)
    await setSpecInfoToList(materialList)
    await numFmtByBasicClass(materialList, {
      toSmallest: false,
      toNum: false
    })
    materialList.forEach((row) => {
      if (row.basicClass === matClsEnum.STEEL_COIL.V) {
        row.printNumber = 1
      } else {
        row.printNumber = row.quantity
      }
    })
    return materialList
  }

  return {
    getDetailMaterialList
  }
}
