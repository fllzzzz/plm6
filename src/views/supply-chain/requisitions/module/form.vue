<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :content-loading="crud.editDetailLoading"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    size="100%"
    custom-class="requisitions-application-record-form"
  >
    <template #titleRight>
      <!-- <store-operation v-if="crud.status.add > CRUD.STATUS.NORMAL" type="crud" /> -->
    </template>
    <template #content>
      <div class="requisitions-application-select">
        <common-radio-button v-model="form.materialType" :options="materialPurchaseClsEnum.ENUM" type="enum" size="small" />
        <common-radio-button
          v-if="isManufactured"
          v-model="form.finishedProductType"
          :disabledVal="[manufClsEnum.ENCL_MANUFACTURED.V]"
          :options="manufClsEnum.ENUM"
          type="enum"
          size="small"
        />
        <common-radio-button v-if="!isManufactured" v-model="form.type" :options="preparationTypeEnum.ENUM" type="enum" size="small" />
        <project-cascader v-model="form.projectId" :disabled="form.type === preparationTypeEnum.PUBLIC.V" clearable style="width: 400px" />
      </div>
      <component v-if="crud.status.cu > CRUD.STATUS.NORMAL" :is="comp" :detail="form" @success="handleSuccess" :isEdit="isEdit" />
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, watch } from 'vue'
import { regForm } from '@compos/use-crud'
// import { STEEL_ENUM } from '@/settings/config'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { isNotBlank } from '@data-type/index'
import { materialPurchaseClsEnum, manufClsEnum } from '@enum-ms/classification'
import { preparationTypeEnum, requisitionModeEnum } from '@enum-ms/wms'

import ProjectCascader from '@comp-base/project-cascader.vue'
import SteelApplication from './application/steel/index'
import AuxMatApplication from './application/auxiliary-material/index'
import ManufApplication from './application/manufactured/index'
// import StoreOperation from '@crud/STORE.operation.vue'

const defaultForm = {
  materialType: materialPurchaseClsEnum.STEEL.V, // 申购类型
  type: preparationTypeEnum.PROJECT.V, // 备料类型
  finishedProductType: manufClsEnum.STRUC_MANUFACTURED.V, // 制成品类型
  projectId: undefined, // 项目id （项目备料可以多选）
  arrivalTime: '', // 到厂时间
  serialNumber: '', // 申购单号
  steelPlateList: [],
  sectionSteelList: [],
  steelCoilList: [],
  remark: '', // 备注
  list: [] // 汇总列表
}

const { CRUD, crud, form } = regForm(defaultForm)

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

const isManufactured = computed(() => Boolean(form.materialType & materialPurchaseClsEnum.MANUFACTURED.V))

watch(
  () => form.type,
  (val) => {
    if (val & preparationTypeEnum.PUBLIC.V) {
      form.projectId = undefined
    }
  }
)

watch(
  () => form.materialType,
  (val, oldVal) => {
    if (val && oldVal && val !== oldVal) {
      form.list = []
    }
  }
)

watch(
  () => isManufactured,
  (val) => {
    if (isManufactured.value) {
      form.type = preparationTypeEnum.PROJECT.V
    }
  }
)

const comp = computed(() => {
  switch (form.materialType) {
    case materialPurchaseClsEnum.STEEL.V:
      return SteelApplication
    case materialPurchaseClsEnum.MATERIAL.V:
      return AuxMatApplication
    case materialPurchaseClsEnum.MANUFACTURED.V:
      return ManufApplication
    default:
      return undefined
  }
})

CRUD.HOOK.beforeEditDetailLoaded = async (crud, detail) => {
  if (isNotBlank(detail.project)) {
    detail.projectId = detail.project?.id
  }
  if (!isManufactured.value) {
    if (!detail.originInventoryInfo) detail.originInventoryInfo = {}
    detail.list = detail.detailList.map((v) => {
      if (v.materialInventoryId) {
        v.requisitionMode = requisitionModeEnum.USE_INVENTORY.V
        if (!detail.originInventoryInfo[v.materialInventoryId]) {
          detail.originInventoryInfo[v.materialInventoryId] = {
            quantity: v.materialFreezeNum?.quantity,
            frozenQuantity: v.materialFreezeNum?.frozenQuantity - v.quantity
          }
        } else {
          detail.originInventoryInfo[v.materialInventoryId].frozenQuantity -= v.quantity
          if (detail.originInventoryInfo[v.materialInventoryId].frozenQuantity < 0) {
            detail.originInventoryInfo[v.materialInventoryId].frozenQuantity = 0
          }
        }
      } else {
        v.requisitionMode = requisitionModeEnum.PURCHASE.V
      }
      return v
    })
    await setSpecInfoToList(detail.list)
    detail.list = await numFmtByBasicClass(detail.list, {
      toSmallest: false,
      toNum: true
    })
  } else {
    detail.list = detail.detailList.map((v) => {
      return v
    })
  }
  console.log(detail.projectId, 'projectId')
}

function handleSuccess() {
  crud.cancelCU()
  crud.refresh()
}
</script>

<style lang="scss">
.requisitions-application-record-form {
  .el-drawer__body {
    padding: 0;
  }
}
</style>
<style lang="scss" scoped>
.requisitions-application-select {
  padding: 0 20px 10px;
  display: flex;
  align-content: center;
  > * {
    margin-right: 10px;
  }
}
</style>
