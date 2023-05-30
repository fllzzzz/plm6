<template>
  <common-table
    :header-cell-style="() => `background:#fff;font-weight: bold;color: #333333;`"
    :data="compareList"
    :data-format="columnsDataFormat"
    style="width: 100%"
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column
      v-if="Boolean(props.showOrderNumber)"
      label="排产工单"
      prop="orderNumber"
      show-overflow-tooltip
      align="center"
      min-width="100"
    />
    <el-table-column label="区域" prop="area.name" show-overflow-tooltip align="center" min-width="100" />
    <el-table-column :label="`原${props.typeTitle}信息`" align="center">
      <el-table-column label="编号" prop="oldSerialNumber" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="规格" prop="oldSpecification" show-overflow-tooltip align="center" min-width="120" />
      <el-table-column label="材质" prop="oldMaterial" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="长度(mm)" prop="oldLength" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column v-if="Boolean(props.showStatus)" label="状态" align="center" width="80">
        <template #default>
          <el-tag type="danger" effect="plain">删除</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="数量" prop="oldQuantity" show-overflow-tooltip align="center" width="90" />
    </el-table-column>
    <el-table-column v-if="!Boolean(props.unShowNew)" :label="`变更${props.typeTitle}信息`" align="center">
      <el-table-column label="编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column label="长度(mm)" prop="length" show-overflow-tooltip align="center" min-width="100" />
      <el-table-column v-if="Boolean(props.showStatus)" label="状态" align="center" width="80">
        <template #default>
          <el-tag type="success" effect="plain">新增</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="数量" prop="quantity" show-overflow-tooltip align="center" width="90" />
    </el-table-column>
    <template v-if="Boolean(props.showChange)">
      <el-table-column label="部件变更" prop="a" show-overflow-tooltip align="center" width="90" />
      <el-table-column label="零件变更" prop="p" show-overflow-tooltip align="center" width="90" />
    </template>
    <el-table-column
      v-if="Boolean(props.showHandleType)"
      label="处理方案"
      prop="handleType"
      show-overflow-tooltip
      align="center"
      min-width="100"
    />
    <template v-if="Boolean(props.showProductionQty)">
      <el-table-column label="未生产" prop="quantity" show-overflow-tooltip align="center" width="90" />
      <el-table-column label="生产中" prop="quantity" show-overflow-tooltip align="center" width="90" />
    </template>
    <el-table-column
      v-if="Boolean(props.showShippedQty)"
      label="已发运数量"
      prop="cargoListQuantity"
      show-overflow-tooltip
      align="center"
      width="90"
    />
    <el-table-column v-if="Boolean(props.showHandleQty)" label="处理数量" prop="quantity" show-overflow-tooltip align="center" width="90" />
  </common-table>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { assembleHandleMethodEnum } from './common'

const props = defineProps({
  compareList: {
    type: Array,
    default: () => []
  },
  typeTitle: {
    type: String,
    default: '构件'
  },
  showOrderNumber: {
    type: Boolean,
    default: false
  },
  showStatus: {
    type: Boolean,
    default: false
  },
  unShowNew: {
    type: Boolean,
    default: false
  },
  showChange: {
    type: Boolean,
    default: false
  },
  showHandleType: {
    type: Boolean,
    default: false
  },
  showProductionQty: {
    type: Boolean,
    default: false
  },
  showHandleQty: {
    type: Boolean,
    default: false
  },
  showShippedQty: {
    type: Boolean,
    default: false
  }
})

const columnsDataFormat = ref([['handleType', ['parse-enum', assembleHandleMethodEnum, { bit: true, split: ' 、 ' }]]])
</script>

<style lang="scss" scoped></style>
