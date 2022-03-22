<template>
  <el-form ref="form" :model="data" label-width="40px" style="display: flex" label-position="left">
    <el-form-item label="项目" style="margin-right: 20px">
      <span v-parse-project="{ project: data.project, onlyShortName: true }" />
    </el-form-item>
    <el-form-item label="单体" style="margin-right: 20px">
      <span>{{ data.monomer?.name }}</span>
    </el-form-item>
    <el-form-item label="区域">
      <span>{{ data.areaDetail?.name }}</span>
    </el-form-item>
  </el-form>
  <common-table :dataFormat="productFormat[productType]" :data="[productData]">
    <productType-full-info-columns
      :productType="productType"
      :unitNewLine="false"
      :unShowField="['totalNetWeight', 'totalGrossWeight', 'remark']"
    />
    <el-table-column prop="oldQuantity" :show-overflow-tooltip="true" label="原清单数量" width="100" align="center">
      <template #default="{ row }">
        <span>{{ row.oldQuantity }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="newQuantity" :show-overflow-tooltip="true" label="变更后清单数量" width="110" align="center">
      <template #default="{ row }">
        <span>{{ row.newQuantity }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showTaskQ" prop="taskQuantity" :show-overflow-tooltip="true" label="任务数量" width="100" align="center">
      <template #default="{ row }">
        <span>{{ row.taskQuantity }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showSchedulingQ"
      prop="totalSchedulingQuantity"
      :show-overflow-tooltip="true"
      label="排产数量"
      width="100"
      align="center"
    >
      <template #default="{ row }">
        <span>{{ row.totalSchedulingQuantity }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showInProductionQ"
      prop="totalInProductionQuantity"
      :show-overflow-tooltip="true"
      label="已生产数量"
      width="100"
      align="center"
    >
      <template #default="{ row }">
        <span>{{ row.totalInProductionQuantity }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showHandleQ" prop="handleQuantity" :show-overflow-tooltip="true" label="需处理数量" width="100" align="center">
      <template #default="{ row }">
        <span>{{ row.handleQuantity }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { productInfo } from '@/api/mes/changed-manage/common'
import { computed, defineProps, watch, ref } from 'vue'
import { convertUnits } from '@/utils/convert/unit'

import { productFormat } from '@/utils/columns-format/mes'
import productTypeFullInfoColumns from '@comp-mes/table-columns/productType-full-info-columns'

const props = defineProps({
  data: {
    type: Object
  },
  productType: {
    type: Number
  },
  showTaskQ: {
    type: Boolean,
    default: false
  },
  showSchedulingQ: {
    type: Boolean,
    default: false
  },
  showInProductionQ: {
    type: Boolean,
    default: false
  },
  showHandleQ: {
    type: Boolean,
    default: false
  }
})

const productId = computed(() => {
  return props.data?.productId
})

const productData = ref({})
const loading = ref(false)

watch(
  () => productId.value,
  () => {
    fetch()
  },
  { immediate: true }
)

async function fetch() {
  try {
    loading.value = true
    const info = await productInfo({
      id: props.data.productId,
      productType: props.productType
    })
    info.surfaceArea = info.surfaceArea && convertUnits(info.surfaceArea, 'mm2', 'm2')
    productData.value = Object.assign({}, info, props.data)
  } catch (error) {
    productData.value = Object.assign({}, props.data)
    console.log('获取产品信息', error)
  } finally {
    loading.value = false
  }
}
</script>
