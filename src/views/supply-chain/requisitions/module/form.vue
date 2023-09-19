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
    <template #content>
      <div class="requisitions-application-select">
        <common-radio-button
          v-model="form.materialType"
          :options="materialPurchaseClsEnum.ENUM"
          type="enum"
          size="small"
        />
        <common-radio-button
          v-model="form.type"
          :options="preparationTypeEnum.ENUM"
          type="enum"
          size="small"
        />
        <project-cascader
          v-if="form.type === preparationTypeEnum.PROJECT.V"
          v-model="form.projectId"
          clearable
          multiple
          collapse-tags
          style="width: 400px;height: 32px;"
        />
        <project-cascader
          v-else
          v-model="form.projectId"
          clearable
          :disabled="form.type === preparationTypeEnum.PUBLIC.V"
          style="width: 400px;"
        />
      </div>
      <component :is="comp" :detail="form" @success="handleSuccess" />
    </template>
  </common-drawer>
</template>

<script setup>
import { computed } from 'vue'
import { regForm } from '@compos/use-crud'
import { STEEL_ENUM } from '@/settings/config'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialPurchaseClsEnum } from '@enum-ms/classification'
import { preparationTypeEnum } from '@enum-ms/wms'

import ProjectCascader from '@comp-base/project-cascader.vue'
import SteelApplication from './application/steel/index'
import AuxMatApplication from './application/auxiliary-material/index'
import OtherApplication from './application/other/index'

const defaultForm = {
  materialType: materialPurchaseClsEnum.STEEL.V, // 申购类型
  type: preparationTypeEnum.PROJECT.V, // 备料类型
  projectId: [], // 项目id （项目备料可以多选）
  arrivalTime: '', // 到厂时间
  serialNumber: '', // 申购单号
  steelPlateList: [],
  sectionSteelList: [],
  steelCoilList: [],
  remark: '', // 备注
  list: [] // 汇总列表
}

const { CRUD, crud, form } = regForm(defaultForm)

const comp = computed(() => {
  switch (form.materialType) {
    case matClsEnum.STEEL_PLATE.V:
    case matClsEnum.SECTION_STEEL.V:
    case matClsEnum.STEEL_COIL.V:
      return SteelApplication
    case matClsEnum.MATERIAL.V:
      return AuxMatApplication
    case matClsEnum.OTHER.V:
      return OtherApplication
    default:
      if (form.materialType & STEEL_ENUM) return SteelApplication
      return undefined
  }
})

CRUD.HOOK.beforeEditDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.list)
  detail.list = await numFmtByBasicClass(detail.list, {
    toSmallest: false,
    toNum: true
  })
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
