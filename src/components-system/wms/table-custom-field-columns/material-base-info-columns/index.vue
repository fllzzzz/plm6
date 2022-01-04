<template>
  <el-table-column v-if="showIndex" label="序号" type="index" align="center" width="55" :fixed="fixed">
    <template #default="{ row, $index }">
      <div v-if="row.overTipColor" class="left-triangle-tip" :style="{ 'border-left-color': row.overTipColor }" />
      <table-cell-tag v-if="showPartyA && partyAPosition === 'index'" :show="!!getInfo(row, 'boolPartyA')" name="甲供" :color="TAG_PARTY_DEF_COLOR" />
      <span>{{ $index + 1 }}</span>
    </template>
  </el-table-column>
  <slot name="afterIndex" />
  <el-table-column
    v-if="showProject"
    :prop="`${field}.project`"
    label="项目"
    align="left"
    min-width="120px"
    fixed="left"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <table-cell-tag v-if="showPartyA && partyAPosition === 'project'" :show="!!getInfo(row, 'boolPartyA')" name="甲供" :color="TAG_PARTY_DEF_COLOR" :offset="15" />
      <span v-parse-project="{ project: getInfo(row, 'project'), onlyShortName: true }" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showSerialNumber" :prop="`${field}.serialNumber`" label="编号" align="center" width="110px" :fixed="fixed" show-overflow-tooltip>
    <template #default="{ row }">
      <table-cell-tag
        v-if="showPartyATransfer && getInfo(row, 'partyATransferType')"
        :name="partyAMatTransferEnum.VL[getInfo(row, 'partyATransferType')]"
        :color="partyAMatTransferEnum.V[getInfo(row, 'partyATransferType')].COLOR"
        :offset="15"
      />
      <table-cell-tag
        v-if="showOutboundMode && getInfo(row, 'materialOutboundMode') === materialOutboundModeEnum.HALF.V"
        :name="materialOutboundModeEnum.VL[getInfo(row, 'materialOutboundMode')]"
        :color="materialOutboundModeEnum.V[getInfo(row, 'materialOutboundMode')].COLOR"
        :offset="15"
      />
      <span v-empty-text>{{ getInfo(row, 'serialNumber') }}</span>
    </template>
  </el-table-column>
  <component
    :is="comp"
    :columns="props.columns"
    :basic-class="props.basicClass"
    :spec-merge="props.specMerge"
    :show-factory="props.showFactory"
    :fixed="fixed"
    :field="field"
    v-bind="$attrs"
  />
</template>

<script setup>
import { defineProps, computed, provide } from 'vue'
import { TAG_PARTY_DEF_COLOR } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { materialOutboundModeEnum, partyAMatTransferEnum } from '@/utils/enum/modules/wms'
import { isBlank } from '@/utils/data-type'

import TableCellTag from '@/components-system/common/table-cell-tag/index.vue'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'
import rawMat from './module/raw-mat.vue'

const props = defineProps({
  specMerge: {
    // 规格合并,规格与 厚宽长颜色等合并为一个字段
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  showIndex: {
    // 显示 “序号”
    type: Boolean,
    default: true
  },
  showPartyA: {
    // 显示 “甲供”
    type: Boolean,
    default: true
  },
  partyAPosition: {
    type: String,
    default: 'index' // index / project
  },
  showPartyATransfer: {
    // 显示 甲供调拨类型
    type: Boolean,
    default: false
  },
  showOutboundMode: {
    // 显示 出库方式 （整料半出）
    type: Boolean,
    default: false
  },
  showProject: {
    // 显示项目
    type: Boolean,
    default: false
  },
  fixed: {
    // 定位
    type: String
  },
  field: {
    // 字段
    type: String,
    default: 'material'
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlate
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteel
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoil
    case rawMatClsEnum.MATERIAL.V:
      return auxMat
    case rawMatClsEnum.GAS.V:
      return gas
    default:
      return rawMat
  }
})

const showSerialNumber = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.serialNumber`))

// 根据传入的物料字段获取信息
function getInfo(row, field) {
  const materialField = props.field
  if (isBlank(row) || isBlank(row[materialField])) return
  return !field ? row[materialField] : row[materialField][field]
}
provide('getInfo', getInfo)
</script>
