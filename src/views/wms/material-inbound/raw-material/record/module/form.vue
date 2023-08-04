<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :content-loading="crud.editDetailLoading"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    size="90%"
    custom-class="raw-mat-inbound-application-record-form"
  >
    <template #content>
      <component :is="comp" :detail="form" edit @success="handleSuccess" />
    </template>
  </common-drawer>
</template>

<script setup>
import { computed } from 'vue'
import { regForm } from '@compos/use-crud'
import { STEEL_ENUM } from '@/settings/config'
import { orderSupplyTypeEnum } from '@enum-ms/wms'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { deepClone, toPrecision } from '@/utils/data-type'
import { isNotBlank } from '@data-type/index'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import SteelApplication from '@/views/wms/material-inbound/raw-material/application/steel/index.vue'
import AuxMatApplication from '@/views/wms/material-inbound/raw-material/application/auxiliary-material/index.vue'
import OtherApplication from '@/views/wms/material-inbound/raw-material/application/other/index.vue'
import GasApplication from '@/views/wms/material-inbound/raw-material/application/gas/index.vue'
import ManufApplication from '@/views/wms/material-inbound/manufactured/index.vue'

const { CRUD, crud, form } = regForm()

const { baseUnit } = useMatBaseUnit() // 当前分类基础单位

const comp = computed(() => {
  switch (form.basicClass) {
    case matClsEnum.STEEL_PLATE.V:
    case matClsEnum.SECTION_STEEL.V:
    case matClsEnum.STEEL_COIL.V:
      return SteelApplication
    case matClsEnum.MATERIAL.V:
      return AuxMatApplication
    case matClsEnum.GAS.V:
      return GasApplication
    case matClsEnum.STRUC_MANUFACTURED.V:
    case matClsEnum.ENCL_MANUFACTURED.V:
      return ManufApplication
    case matClsEnum.OTHER.V:
      return OtherApplication
    default:
      if (form.basicClass & STEEL_ENUM) return SteelApplication
      return undefined
  }
})

CRUD.HOOK.beforeEditDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.list)
  detail.list = await numFmtByBasicClass(detail.list, {
    toSmallest: false,
    toNum: true
  })
  detail.list.forEach((v) => {
    v.warehouseName = v.workshop?.name || v.warehouseName
    v.workshopId = v.workshop?.id || v.workshopId
    v.warehouseId = v.warehouse?.id || v.warehouseId
    v.projectId = v.project?.id || v.projectId
  })
  // 物流信息
  detail.logistics = detail.logisticsOrder
  if (detail.supplyType !== orderSupplyTypeEnum.PARTY_A.V && isNotBlank(detail.purchaseOrder.details)) {
    detail.originList = deepClone(detail.list)
    await calcTheoryWeight(detail.originList)
    detail.list = []
    detail.editObj = {}
    detail.originList.forEach((v) => {
      if (v.basicClass & (matClsEnum.STEEL_PLATE.V | matClsEnum.SECTION_STEEL.V)) {
        v.theoryTotalWeight = toPrecision(v.theoryWeight * v.quantity, baseUnit.value[v.basicClass].weight.precision)
      }
      if (!detail.editObj[v.mergeId]) {
        detail.editObj[v.mergeId] = {
          ...v,
          applyPurchaseObj: {
            [v.applyPurchaseId]: [{ ...v }]
          },
          isSelected: true
        }
      } else {
        if (!detail.editObj[v.mergeId].applyPurchaseObj[v.applyPurchaseId]) {
          detail.editObj[v.mergeId].applyPurchaseObj[v.applyPurchaseId] = [{ ...v }]
        } else {
          detail.editObj[v.mergeId].applyPurchaseObj[v.applyPurchaseId].push({ ...v })
        }
        detail.editObj[v.mergeId].quantity += v.quantity
        detail.editObj[v.mergeId].mete += v.mete
      }
    })
  }
  console.log(detail)
}

function handleSuccess() {
  crud.cancelCU()
  crud.refresh()
}
</script>

<style lang="scss" scoped>
::v-deep(.inbound-application-container) {
  .header {
    padding: 0 0 10px 0;
  }
  .main-content {
    padding: 0;
  }
}
</style>
