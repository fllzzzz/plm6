<template>
  <el-form-item label="类型" prop="transferType">
    <common-radio
      v-model="currentForm.transferType"
      :options="transferNormalTypeEnum.ENUM"
      :disabled-val="disabledTransferType"
      type="enum"
      size="small"
    />
  </el-form-item>
  <template v-if="showProjectSelect">
    <el-form-item label="项目" prop="projectId">
      <project-cascader v-model="currentForm.projectId" clearable style="width: 100%" />
    </el-form-item>
    <el-form-item label="单体" prop="monomerId">
      <common-select
        v-model="currentForm.monomerId"
        :options="currentForm.projectId && projectMap?.[currentForm.projectId]?.children || []"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        clearable
        type="other"
        placeholder="可选择单体"
        style="width: 100%"
      />
    </el-form-item>
    <el-form-item label="区域" prop="areaId">
      <common-select
        v-model="currentForm.areaId"
        :options="currentForm.monomerId && monomerMap?.[currentForm.monomerId]?.children || []"
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        clearable
        type="other"
        placeholder="可选择区域"
        style="width: 100%"
      />
    </el-form-item>
  </template>
  <el-form-item v-if="showFactoryAndWare" label="工厂" prop="factoryId">
    <factory-select v-model="currentForm.factoryId" placeholder="工厂" style="width: 100%" />
  </el-form-item>
  <el-form-item v-if="showFactoryAndWare" label="仓库" prop="warehouseId">
    <warehouse-select
      v-model="currentForm.warehouseId"
      :factory-id="currentForm.factoryId"
      :basic-class="material.basicClass"
      placeholder="存储位置"
      style="width: 100%"
    />
  </el-form-item>
  <el-form-item :label="`数量(${material.outboundUnit})`" prop="quantity">
    <div class="flex-rsc" style="width: 100%">
      <common-input-number
        v-model="currentForm.quantity"
        :min="0"
        :precision="material.outboundUnitPrecision"
        :max="material.corOperableQuantity"
        controls-position="right"
        style="flex: auto"
      />
      <span class="text-clickable set-max-text" style="flex: none" @click="setMaxQuantity">全部调拨</span>
    </div>
  </el-form-item>
  <el-form-item label="备注" prop="remark">
    <el-input
      v-model.trim="currentForm.remark"
      type="textarea"
      :autosize="{ minRows: 3, maxRows: 3 }"
      maxlength="200"
      show-word-limit
      placeholder="备注"
      style="width: 100%"
    />
  </el-form-item>
</template>

<script setup>
import { defineProps, computed, watchEffect, ref } from 'vue'
import { transferNormalTypeEnum } from '@/utils/enum/modules/wms'

import ProjectCascader from '@comp-base/project-cascader.vue'
import FactorySelect from '@/components-system/base/factory-select.vue'
import WarehouseSelect from '@/components-system/wms/warehouse-select.vue'
import useProjectTree from '@compos/store/use-project-tree'

const props = defineProps({
  material: {
    // 物料出库信息
    type: Object
  },
  form: {
    // 物料出库表单
    type: Object
  }
})

const { projectMap, monomerMap } = useProjectTree()

// 表单
const currentForm = ref({
  // 当前表单字段
  transferType: undefined, // 调拨类型
  projectId: undefined, // 项目id
  monomerId: undefined, // 单体id
  areaId: undefined, // 区域id
  factoryId: undefined, // 工厂id
  warehouseId: undefined, // 仓库id
  quantity: undefined, // 数量
  remark: undefined // 备注
})
// 监听
watchEffect(() => {
  currentForm.value = props.form
})

// 禁用调拨类型
const disabledTransferType = computed(() => {
  if (props.material && props.material.boolPartyA) {
    return []
  }
  return [transferNormalTypeEnum.RETURN_PARTY_A.V]
})

// 显示项目选择 ：项目仓，且配置为可出库到其他项目的情况下可选择
const showProjectSelect = computed(() => {
  return currentForm.value.transferType === transferNormalTypeEnum.PROJECT_WARE.V
})

const showFactoryAndWare = computed(() => {
  return currentForm.value.transferType !== transferNormalTypeEnum.RETURN_PARTY_A.V
})

// 设置最大数量
function setMaxQuantity() {
  currentForm.value.quantity = props.material.corOperableQuantity
}
</script>

<style lang="scss" scoped>
.set-max-text {
  margin-left: 10px;
}
</style>
